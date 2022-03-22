const WASI_ESUCCESS = 0
const WASI_STDOUT_FILENO = 1
const WASI_STDERR_FILENO = 2

type U32 = number

const CLOCK = {
  REALTIME: 0,
  MONOTONIC: 1,
}

export class WASI {
  memory: WebAssembly.Memory

  setMemory(memory: WebAssembly.Memory) {
    this.memory = memory
  }

  getDataView(): DataView {
    return new DataView(this.memory.buffer)
  }

  exports() {
    return {
      proc_exit() {},

      fd_prestat_get() {},
      fd_prestat_dir_name() {},

      fd_write: (fd: U32, iovs: U32, iovsLen: U32, nwritten: U32) => {
        const view = this.getDataView()
        let written = 0
        let bufferBytes = []

        const buffers = Array.from({ length: iovsLen }, (_, i) => {
          const ptr = iovs + i * 8
          const buf = view.getUint32(ptr, !0)
          const bufLen = view.getUint32(ptr + 4, !0)

          return new Uint8Array(
            this.memory.buffer, buf, bufLen
          )
        })

        for (const iov of buffers) {
          for (var b = 0; b < iov.byteLength; b++)
            bufferBytes.push(iov[b])

          written += b
        }

        if (fd === WASI_STDOUT_FILENO)
          console.log(String.fromCharCode.apply(null, bufferBytes))
        else if (fd === WASI_STDERR_FILENO)
          console.warn(String.fromCharCode.apply(null, bufferBytes))

        view.setUint32(nwritten, written, !0)

        return WASI_ESUCCESS;
      },

      fd_close() {},
      fd_read() {},

      path_open() {},
      path_rename() {},
      path_create_directory() {},
      path_remove_directory() {},
      path_unlink_file() {},

      fd_filestat_get() {},

      random_get: (buf_ptr: number, buf_len: number): number => {
        const buffer = new Uint8Array(this.memory.buffer, buf_ptr, buf_len)
        crypto.getRandomValues(buffer)

        return 0;
      },

      clock_time_get: (
        clock_id: number,
        _precision: bigint,
        timestamp_out: number,
      ) => {
        const view = this.getDataView()

        switch (clock_id) {
          case CLOCK.REALTIME:
          case CLOCK.MONOTONIC: {
            const t = BigInt(Date.now()) * BigInt(1e6)
            view.setBigUint64(timestamp_out, t, true)
            break
          }

          default:
            throw new Error("unhandled clock type")
        }

        return 0
      },
    }
  }
}
