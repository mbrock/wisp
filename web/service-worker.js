// This file is part of Wisp.
//
// Wisp is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version
// 3 of the License, or (at your option) any later version.
//
// Wisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General
// Public License along with Wisp. If not, see
// <https://www.gnu.org/licenses/>.

const cacheName = "wisp-workbench-v1";
const assets = [
  "./",
  "./index.html",
  "./index.css?dev",
  "./index.js?dev",
  "./wisp.js",
  "./wasi.js",
  "./js.wisp",
  "./dexp.wisp",
  "./demo.wisp",
  "./dist/wisp.wasm",
  "./lib/codemirror.js",
  "./lib/git.js",
  "./lib/idom.js",
  "./lib/wisplang.js",
];
const assetURLs = new Set(
  assets.map(
    (asset) => new URL(asset, self.registration.scope).href
  )
);

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open(cacheName)
      .then((cache) => cache.addAll(assets))
      .then(() => self.skipWaiting())
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((names) =>
        Promise.all(
          names
            .filter((name) => name !== cacheName)
            .map((name) => caches.delete(name))
        )
      )
      .then(() => self.clients.claim())
  );
});

self.addEventListener("fetch", (event) => {
  const { request } = event;
  const url = new URL(request.url);
  const navigation = request.mode === "navigate";
  if (
    request.method !== "GET" ||
    url.origin !== self.location.origin ||
    (!navigation && !assetURLs.has(url.href))
  ) {
    return;
  }

  const network = fetch(request);
  event.respondWith(
    network.catch(() =>
      caches.match(
        navigation
          ? new URL("./", self.registration.scope)
          : request
      )
    )
  );
  event.waitUntil(
    network
      .then((response) =>
        response.ok
          ? caches
              .open(cacheName)
              .then((cache) =>
                cache.put(request, response.clone())
              )
          : undefined
      )
      .catch(() => undefined)
  );
});
