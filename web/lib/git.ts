import fs from "@isomorphic-git/lightning-fs"
import * as git from "isomorphic-git"
import * as http from "isomorphic-git/http/web"
import { Buffer } from "buffer"

window.Buffer = Buffer

export default {
  fs, git, http
}
