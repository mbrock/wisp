// -*- fill-column: 64; -*-
//
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
//

const WASI_ESUCCESS = 0;
const WASI_STDOUT_FILENO = 1;
const WASI_STDERR_FILENO = 2;
const WASI_EBADF = 8;
const WASI_ENOSYS = 52;

const CLOCK = {
  REALTIME: 0,
  MONOTONIC: 1,
};

export default class WASI {
  memory;
  buffers;

  constructor() {
    this.buffers = {
      [WASI_STDOUT_FILENO]: [],
      [WASI_STDERR_FILENO]: [],
    };
  }

  setMemory(memory) {
    this.memory = memory;
  }

  getDataView() {
    return new DataView(this.memory.buffer);
  }

  exports() {
    return {
      proc_exit() {},

      environ_sizes_get: (count, size) => {
        const view = this.getDataView();
        view.setUint32(count, 0, true);
        view.setUint32(size, 0, true);
        return WASI_ESUCCESS;
      },

      environ_get() {
        return WASI_ESUCCESS;
      },

      fd_prestat_get() {
        return WASI_EBADF;
      },

      fd_prestat_dir_name() {
        return WASI_EBADF;
      },

      fd_write: (fd, iovs, iovsLen, nwritten) => {
        const view = this.getDataView();
        let written = 0;

        const buffers = Array.from({ length: iovsLen }, (_, i) => {
          const ptr = iovs + i * 8;
          const buf = view.getUint32(ptr, !0);
          const bufLen = view.getUint32(ptr + 4, !0);

          return new Uint8Array(this.memory.buffer, buf, bufLen);
        });

        // XXX: verify that this handles multiple lines correctly
        for (let iov of buffers) {
          const newline = 10;
          let i = 0;
          let newlineIndex = iov.lastIndexOf(newline, i);
          if (newlineIndex > -1) {
            let line = "",
              decoder = new TextDecoder();

            for (let buffer of this.buffers[fd]) {
              line += decoder.decode(buffer, { stream: true });
            }

            line += decoder.decode(iov.slice(0, newlineIndex));

            if (fd === WASI_STDOUT_FILENO) console.log(line);
            else if (fd === WASI_STDERR_FILENO) console.warn(line);

            this.buffers[fd] = [iov.slice(newlineIndex + 1)];
            i = newlineIndex + 1;
          }

          this.buffers[fd].push(new Uint8Array(iov.slice(i)));

          written += iov.byteLength;
        }

        view.setUint32(nwritten, written, !0);

        return WASI_ESUCCESS;
      },

      fd_pwrite: (fd, iovs, iovsLen, offset, nwritten) => {
        // Let pwrite append for stdio streams.
        // "You Only Live Once"
        if (fd === WASI_STDOUT_FILENO || fd === WASI_STDERR_FILENO) {
          return this.exports().fd_write(fd, iovs, iovsLen, nwritten);
        }

        return WASI_EBADF;
      },

      fd_close() {},
      fd_read() {},
      fd_pread() {
        // "I was in Nashville, Tennessee last year.
        // After the show I went to a Waffle House.
        // I'm not proud of it, I was hungry.
        // And I'm alone, I'm eating and I'm reading a book, right?
        // Waitress walks over to me: 'Hey, whatcha readin' for?'
        // Isn't that the weirdest fuckin' question you've ever heard?
        // Not what am I reading, but what am I reading FOR?
        // Well, goddamnit, ya stumped me!"

        return WASI_EBADF;
      },

      fd_seek() {},

      fd_fdstat_get() {},
      fd_fdstat_set_flags() {},
      fd_filestat_set_times() {
        return WASI_ENOSYS;
      },
      fd_filestat_set_size() {
        return WASI_ENOSYS;
      },
      fd_sync() {
        return WASI_ESUCCESS;
      },
      fd_readdir() {
        return WASI_EBADF;
      },

      path_open() {},
      path_link() {
        return WASI_ENOSYS;
      },
      path_readlink() {
        return WASI_ENOSYS;
      },
      path_symlink() {
        return WASI_ENOSYS;
      },
      path_rename() {},
      path_create_directory() {},
      path_remove_directory() {},
      path_unlink_file() {},
      path_filestat_get() {
        return WASI_ENOSYS;
      },

      fd_filestat_get() {},

      poll_oneoff() {
        return WASI_ENOSYS;
      },

      clock_res_get: (_clock_id, resolution_out) => {
        this.getDataView().setBigUint64(resolution_out, 1_000_000n, true);
        return WASI_ESUCCESS;
      },

      random_get: (buf_ptr, buf_len) => {
        const buffer = new Uint8Array(this.memory.buffer, buf_ptr, buf_len);
        crypto.getRandomValues(buffer);

        return 0;
      },

      clock_time_get: (clock_id, _precision, timestamp_out) => {
        const view = this.getDataView();

        switch (clock_id) {
          case CLOCK.REALTIME:
          case CLOCK.MONOTONIC: {
            const t = BigInt(Date.now()) * BigInt(1e6);
            view.setBigUint64(timestamp_out, t, true);
            break;
          }

          default:
            throw new Error("unhandled clock type");
        }

        return 0;
      },
    };
  }
}
