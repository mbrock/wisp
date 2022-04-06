import { Middleware, Application, Router }
from "https://deno.land/x/oak@v10.5.1/mod.ts"

import { oakCors }
from "https://deno.land/x/cors@v1.2.2/mod.ts"

import { format }
from "https://deno.land/std@0.130.0/datetime/mod.ts"

import { encode as encodeAsBase32 }
from "https://deno.land/std@0.130.0/encoding/base32.ts"

import * as jose
from 'https://deno.land/x/jose@v4.6.0/index.ts'

const JWKS = jose.createRemoteJWKSet(
  new URL("https://dev-wnks73rd.us.auth0.com/.well-known/jwks.json")
)

const app = new Application<{ userKey: string }>()
const router = new Router

const auth: Middleware = async (ctx, next) => {
  const req  = ctx.request
  const bearer = req.headers.get("Authorization")
  if (!bearer)
    return ctx.throw(401)

  const jwt = bearer.split(" ")[1]
  console.log("verifying jwt", jwt)

  const { payload, protectedHeader } =
    await jose.jwtVerify(jwt, JWKS, {
      issuer: "https://dev-wnks73rd.us.auth0.com/",
      audience: "https://api.wisp.town",
    })

  console.log({ payload, protectedHeader })

  if (!payload["https://wisp.town"])
    return ctx.throw(401)

  const wispData = payload["https://wisp.town"] as { key: string }
  ctx.state.userKey = wispData.key

  await next()
}

async function run(opts: { cwd: string, cmd: string[] }): Promise<boolean> {
  const status = await Deno.run(opts).status()
  return status.code === 0
}

router.post("/git", auth, async (ctx, next) => {
  const now = format(new Date(), "yyyyMMdd")
  const rnd = crypto.getRandomValues(new Uint8Array(6))
  const key = `~${now}.${encodeAsBase32(rnd).replaceAll("=", "")}`
  const cwd = `git/${key}`

  await Deno.mkdir(cwd, { recursive: true })

  if (!await run({ cwd, cmd: ["git", "init"] }))
    return ctx.throw(500)

  if (!await run({
    cwd, cmd: [
      "git", "config", "--add", "wisp.auth.push", ctx.state.userKey,
    ]
  }))
    return ctx.throw(500)

  ctx.response.body = key

  await next()
})

app.use(oakCors())
app.use(router.routes())
app.use(router.allowedMethods())

console.log("listening on http://localhost:8000/")

await app.listen({ port: 8000 })
