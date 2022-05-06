import { serveTls } from "https://deno.land/std@0.138.0/http/server.ts";

function handler(req) {
  const body = JSON.stringify({ message: "NOT FOUND" })
  return new Response(body, {
    status: 404,
    headers: {
      "content-type": "application/json; charset=utf-8",
    },
  })
}

serveTls(handler, {
  port: 443,
  certFile: "./crt.pem",
  keyFile: "./key2.pem",
})
