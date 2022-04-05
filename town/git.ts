import { Middleware, Application, Router }
from "https://deno.land/x/oak/mod.ts"

import { oakCors }
from "https://deno.land/x/cors/mod.ts"

import { serve }
from "https://deno.land/std@0.130.0/http/server.ts"

import { format }
from "https://deno.land/std@0.130.0/datetime/mod.ts"

import { encode as encodeAsBase32 }
from "https://deno.land/std@0.130.0/encoding/base32.ts"

import * as jose
from 'https://deno.land/x/jose@v4.6.0/index.ts'

const JWKS = jose.createRemoteJWKSet(
  new URL("https://dev-wnks73rd.us.auth0.com/.well-known/jwks.json")
)

const app = new Application
const router = new Router

const auth: Middleware = async (ctx, next) => {
  const req  = ctx.request
  
  if (!req.headers.has("Authorization"))
    return ctx.throw(401)

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

  await next()
}

router.get("/foo", auth, ctx => {
  ctx.response.body = "hello"
})

router.post("/git", auth, async ctx => {
  const now = format(new Date(), "yyyyMMdd")
  const rnd = crypto.getRandomValues(new Uint8Array(6))
  const key = `~${now}.${encodeAsBase32(rnd).replaceAll("=", "")}`
  const cwd = `git/${key}`

  await Deno.mkdir(cwd, { recursive: true })
  await Deno.run({ cwd, cmd: ["git", "init"] })
  
  ctx.response.body = key
})

app.use(oakCors())
app.use(router.routes())

console.log("listening on http://localhost:8000/")

await app.listen({ port: 8000 })
