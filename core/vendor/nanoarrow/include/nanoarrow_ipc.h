// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

#ifndef NANOARROW_IPC_H_INCLUDED
#define NANOARROW_IPC_H_INCLUDED

#include "nanoarrow.h"

#ifdef NANOARROW_NAMESPACE

#define ArrowIpcCheckRuntime NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcCheckRuntime)
#define ArrowIpcSharedBufferIsThreadSafe \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcSharedBufferIsThreadSafe)
#define ArrowIpcSharedBufferInit \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcSharedBufferInit)
#define ArrowIpcSharedBufferReset \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcSharedBufferReset)
#define ArrowIpcGetZstdDecompressionFunction \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcGetZstdDecompressionFunction)
#define ArrowIpcGetLZ4DecompressionFunction \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcGetLZ4DecompressionFunction)
#define ArrowIpcSerialDecompressor \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcSerialDecompressor)
#define ArrowIpcSerialDecompressorSetFunction \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcSerialDecompressorSetFunction)
#define ArrowIpcDecoderInit NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderInit)
#define ArrowIpcDecoderReset NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderReset)
#define ArrowIpcDecoderSetDecompressor \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderSetDecompressor)
#define ArrowIpcDecoderPeekHeader \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderPeekHeader)
#define ArrowIpcDecoderVerifyHeader \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderVerifyHeader)
#define ArrowIpcDecoderDecodeHeader \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderDecodeHeader)
#define ArrowIpcDecoderDecodeSchema \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderDecodeSchema)
#define ArrowIpcDecoderDecodeArrayView \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderDecodeArrayView)
#define ArrowIpcDecoderDecodeArray \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderDecodeArray)
#define ArrowIpcDecoderDecodeArrayFromShared \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderDecodeArrayFromShared)
#define ArrowIpcDecoderSetSchema \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderSetSchema)
#define ArrowIpcDecoderSetEndianness \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderSetEndianness)
#define ArrowIpcDecoderPeekFooter \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderPeekFooter)
#define ArrowIpcDecoderVerifyFooter \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderVerifyFooter)
#define ArrowIpcDecoderDecodeFooter \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcDecoderDecodeFooter)
#define ArrowIpcInputStreamInitBuffer \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcInputStreamInitBuffer)
#define ArrowIpcInputStreamInitFile \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcInputStreamInitFile)
#define ArrowIpcInputStreamMove \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcInputStreamMove)
#define ArrowIpcArrayStreamReaderInit \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcArrayStreamReaderInit)
#define ArrowIpcEncoderInit NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcEncoderInit)
#define ArrowIpcEncoderReset NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcEncoderReset)
#define ArrowIpcEncoderFinalizeBuffer \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcEncoderFinalizeBuffer)
#define ArrowIpcEncoderEncodeSchema \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcEncoderEncodeSchema)
#define ArrowIpcEncoderEncodeSimpleRecordBatch \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcEncoderEncodeSimpleRecordBatch)
#define ArrowIpcOutputStreamInitBuffer \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcOutputStreamInitBuffer)
#define ArrowIpcOutputStreamInitFile \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcOutputStreamInitFile)
#define ArrowIpcOutputStreamWrite \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcOutputStreamWrite)
#define ArrowIpcOutputStreamMove \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcOutputStreamMove)
#define ArrowIpcWriterInit NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcWriterInit)
#define ArrowIpcWriterReset NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcWriterReset)
#define ArrowIpcWriterWriteSchema \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcWriterWriteSchema)
#define ArrowIpcWriterWriteArrayView \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcWriterWriteArrayView)
#define ArrowIpcWriterWriteArrayStream \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcWriterWriteArrayStream)
#define ArrowIpcWriterStartFile \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcWriterStartFile)
#define ArrowIpcWriterFinalizeFile \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcWriterFinalizeFile)
#define ArrowIpcFooterInit NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcFooterInit)
#define ArrowIpcFooterReset NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcFooterReset)
#define ArrowIpcEncoderEncodeFooter \
  NANOARROW_SYMBOL(NANOARROW_NAMESPACE, ArrowIpcEncoderEncodeFooter)

#endif

#ifdef __cplusplus
extern "C" {
#endif

/// \defgroup nanoarrow_ipc Nanoarrow IPC extension
///
/// Except where noted, objects are not thread-safe and clients should
/// take care to serialize accesses to methods.
///
/// Because this library is intended to be vendored, it provides full type
/// definitions and encourages clients to stack or statically allocate
/// where convenient.
///
/// @{

/// \brief Metadata version enumerator
enum ArrowIpcMetadataVersion {
  NANOARROW_IPC_METADATA_VERSION_V1,
  NANOARROW_IPC_METADATA_VERSION_V2,
  NANOARROW_IPC_METADATA_VERSION_V3,
  NANOARROW_IPC_METADATA_VERSION_V4,
  NANOARROW_IPC_METADATA_VERSION_V5
};

/// \brief Message type enumerator
enum ArrowIpcMessageType {
  NANOARROW_IPC_MESSAGE_TYPE_UNINITIALIZED,
  NANOARROW_IPC_MESSAGE_TYPE_SCHEMA,
  NANOARROW_IPC_MESSAGE_TYPE_DICTIONARY_BATCH,
  NANOARROW_IPC_MESSAGE_TYPE_RECORD_BATCH,
  NANOARROW_IPC_MESSAGE_TYPE_TENSOR,
  NANOARROW_IPC_MESSAGE_TYPE_SPARSE_TENSOR
};

/// \brief Endianness enumerator
enum ArrowIpcEndianness {
  NANOARROW_IPC_ENDIANNESS_UNINITIALIZED,
  NANOARROW_IPC_ENDIANNESS_LITTLE,
  NANOARROW_IPC_ENDIANNESS_BIG
};

/// \brief Compression type enumerator
enum ArrowIpcCompressionType {
  NANOARROW_IPC_COMPRESSION_TYPE_NONE,
  NANOARROW_IPC_COMPRESSION_TYPE_LZ4_FRAME,
  NANOARROW_IPC_COMPRESSION_TYPE_ZSTD
};

/// \brief Feature flag for a stream that uses dictionary replacement
#define NANOARROW_IPC_FEATURE_DICTIONARY_REPLACEMENT 1

/// \brief Feature flag for a stream that uses compression
#define NANOARROW_IPC_FEATURE_COMPRESSED_BODY 2

/// \brief Checks the nanoarrow runtime to make sure the run/build versions match
NANOARROW_DLL ArrowErrorCode ArrowIpcCheckRuntime(struct ArrowError* error);

/// \brief Get the endianness of the current runtime
static inline enum ArrowIpcEndianness ArrowIpcSystemEndianness(void) {
  uint32_t check = 1;
  char first_byte;
  memcpy(&first_byte, &check, sizeof(char));
  if (first_byte) {
    return NANOARROW_IPC_ENDIANNESS_LITTLE;
  } else {
    return NANOARROW_IPC_ENDIANNESS_BIG;
  }
}

/// \brief A structure representing a reference-counted buffer that may be passed to
/// ArrowIpcDecoderDecodeArrayFromShared().
struct ArrowIpcSharedBuffer {
  struct ArrowBuffer private_src;
};

/// \brief Initialize the contents of a ArrowIpcSharedBuffer struct
///
/// If NANOARROW_OK is returned, the ArrowIpcSharedBuffer takes ownership of
/// src.
NANOARROW_DLL ArrowErrorCode ArrowIpcSharedBufferInit(struct ArrowIpcSharedBuffer* shared,
                                                      struct ArrowBuffer* src);

/// \brief Release the caller's copy of the shared buffer
///
/// When finished, the caller must relinquish its own copy of the shared data
/// using this function. The original buffer will continue to exist until all
/// ArrowArray objects that refer to it have also been released.
NANOARROW_DLL void ArrowIpcSharedBufferReset(struct ArrowIpcSharedBuffer* shared);

/// \brief Check for shared buffer thread safety
///
/// Thread-safe shared buffers require C11 and the stdatomic.h header.
/// If either are unavailable, shared buffers are still possible but
/// the resulting arrays must not be passed to other threads to be released.
NANOARROW_DLL int ArrowIpcSharedBufferIsThreadSafe(void);

/// \brief A user-extensible decompressor
///
/// The ArrowIpcDecompressor is the underlying object that enables decompression in the
/// ArrowIpcDecoder. Its structure allows it to be backed by a multithreaded
/// implementation; however, this is not required and the default implementation does not
/// implement this. An implementation of a decompressor may support more than one
/// ArrowIpcCompressionType.
struct ArrowIpcDecompressor {
  /// \brief Queue a buffer for decompression
  ///
  /// The values pointed to by dst and dst_size after a call to decompress_add
  /// are undefined until the next call to decompress_wait returns NANOARROW_OK.
  ArrowErrorCode (*decompress_add)(struct ArrowIpcDecompressor* decompressor,
                                   enum ArrowIpcCompressionType compression_type,
                                   struct ArrowBufferView src, uint8_t* dst,
                                   int64_t dst_size, struct ArrowError* error);

  /// \brief Wait for any unfinished calls to decompress_add to complete
  ///
  /// Returns NANOARROW_OK if all pending calls completed. Returns ETIMEOUT
  /// if not all remaining calls completed.
  ArrowErrorCode (*decompress_wait)(struct ArrowIpcDecompressor* decompressor,
                                    int64_t timeout_ms, struct ArrowError* error);

  /// \brief Release the decompressor and any resources it may be holding
  ///
  /// Release callback implementations must set the release member to NULL.
  /// Callers must check that the release callback is not NULL before calling
  /// decompress() or release().
  void (*release)(struct ArrowIpcDecompressor* decompressor);

  /// \brief Implementation-specific opaque data
  void* private_data;
};

/// \brief A self-contained decompression function
///
/// For the most common compression type, ZSTD, this function is sufficient to
/// capture the type of decompression that Arrow IPC requires (i.e., decompression
/// where the uncompressed size was recorded). For other compression types, it
/// may be more efficient to implement a full ArrowIpcDecompressor, which allows
/// for persistent state/allocations between decodes.
typedef ArrowErrorCode (*ArrowIpcDecompressFunction)(struct ArrowBufferView src,
                                                     uint8_t* dst, int64_t dst_size,
                                                     struct ArrowError* error);

/// \brief Get the decompression function for ZSTD
///
/// The result will be NULL if nanoarrow was not built with NANOARROW_IPC_WITH_ZSTD.
NANOARROW_DLL ArrowIpcDecompressFunction ArrowIpcGetZstdDecompressionFunction(void);

/// \brief Get the decompression function for LZ4
///
/// The result will be NULL if nanoarrow was not built with NANOARROW_IPC_WITH_LZ4.
NANOARROW_DLL ArrowIpcDecompressFunction ArrowIpcGetLZ4DecompressionFunction(void);

/// \brief An ArrowIpcDecompressor implementation that performs decompression in serial
NANOARROW_DLL ArrowErrorCode
ArrowIpcSerialDecompressor(struct ArrowIpcDecompressor* decompressor);

/// \brief Override the ArrowIpcDecompressFunction used for a specific compression type
///
/// This may be used to inject support for a particular type of decompression if used
/// with a version of nanoarrow with unknown or minimal capabilities.
NANOARROW_DLL ArrowErrorCode
ArrowIpcSerialDecompressorSetFunction(struct ArrowIpcDecompressor* decompressor,
                                      enum ArrowIpcCompressionType compression_type,
                                      ArrowIpcDecompressFunction decompress_function);

/// \brief Decoder for Arrow IPC messages
///
/// This structure is intended to be allocated by the caller,
/// initialized using ArrowIpcDecoderInit(), and released with
/// ArrowIpcDecoderReset(). These fields should not be modified
/// by the caller but can be read following a call to
/// ArrowIpcDecoderPeekHeader(), ArrowIpcDecoderVerifyHeader(), or
/// ArrowIpcDecoderDecodeHeader().
struct ArrowIpcDecoder {
  /// \brief The last verified or decoded message type
  enum ArrowIpcMessageType message_type;

  /// \brief The metadata version as indicated by the current schema message
  enum ArrowIpcMetadataVersion metadata_version;

  /// \brief Buffer endianness as indicated by the current schema message
  enum ArrowIpcEndianness endianness;

  /// \brief Arrow IPC Features used as indicated by the current Schema message
  int32_t feature_flags;

  /// \brief Compression used by the current RecordBatch message
  enum ArrowIpcCompressionType codec;

  /// \brief The number of bytes in the current header message
  ///
  /// This value includes the 8 bytes before the start of the header message
  /// content and any padding bytes required to make the header message size
  /// be a multiple of 8 bytes.
  int32_t header_size_bytes;

  /// \brief The number of bytes in the forthcoming body message.
  int64_t body_size_bytes;

  /// \brief The last decoded Footer
  ///
  /// \warning This API is currently only public for use in integration testing;
  ///          use at your own risk.
  struct ArrowIpcFooter* footer;

  /// \brief Private resources managed by this library
  void* private_data;
};

/// \brief Initialize a decoder
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderInit(struct ArrowIpcDecoder* decoder);

/// \brief Release all resources attached to a decoder
NANOARROW_DLL void ArrowIpcDecoderReset(struct ArrowIpcDecoder* decoder);

/// \brief Set the decompressor implementation used by this decoder
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderSetDecompressor(
    struct ArrowIpcDecoder* decoder, struct ArrowIpcDecompressor* decompressor);

/// \brief Peek at a message header
///
/// The first 8 bytes of an Arrow IPC message are 0xFFFFFFFF followed by the size
/// of the header as a little-endian 32-bit integer. ArrowIpcDecoderPeekHeader() reads
/// these bytes and returns ESPIPE if there are not enough remaining bytes in data to read
/// the entire header message, EINVAL if the first 8 bytes are not valid, ENODATA if the
/// Arrow end-of-stream indicator has been reached, or NANOARROW_OK otherwise.
///
/// Pre-1.0 messages were not prefixed with 0xFFFFFFFF. For these messages, a value
/// of 4 will be placed into prefix_size_bytes; otherwise a value of 8 will be placed
/// into prefix_size_bytes.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderPeekHeader(struct ArrowIpcDecoder* decoder,
                                                       struct ArrowBufferView data,
                                                       int32_t* prefix_size_bytes,
                                                       struct ArrowError* error);

/// \brief Verify a message header
///
/// Runs ArrowIpcDecoderPeekHeader() to ensure data is sufficiently large but additionally
/// runs flatbuffer verification to ensure that decoding the data will not access
/// memory outside of the buffer specified by data. ArrowIpcDecoderVerifyHeader() will
/// also set decoder.header_size_bytes, decoder.body_size_bytes, decoder.metadata_version,
/// and decoder.message_type.
///
/// Returns as ArrowIpcDecoderPeekHeader() and additionally will
/// return EINVAL if flatbuffer verification fails.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderVerifyHeader(struct ArrowIpcDecoder* decoder,
                                                         struct ArrowBufferView data,
                                                         struct ArrowError* error);

/// \brief Decode a message header
///
/// Runs ArrowIpcDecoderPeekHeader() to ensure data is sufficiently large and decodes
/// the content of the message header. If data contains a schema message,
/// decoder.endianness and decoder.feature_flags is set and ArrowIpcDecoderDecodeSchema()
/// can be used to obtain the decoded schema. If data contains a record batch message,
/// decoder.codec is set and a successful call can be followed by a call to
/// ArrowIpcDecoderDecodeArray().
///
/// In almost all cases this should be preceded by a call to
/// ArrowIpcDecoderVerifyHeader() to ensure decoding does not access data outside of the
/// specified buffer.
///
/// Returns EINVAL if the content of the message cannot be decoded or ENOTSUP if the
/// content of the message uses features not supported by this library.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderDecodeHeader(struct ArrowIpcDecoder* decoder,
                                                         struct ArrowBufferView data,
                                                         struct ArrowError* error);

/// \brief Decode an ArrowSchema
///
/// After a successful call to ArrowIpcDecoderDecodeHeader(), retrieve an ArrowSchema.
/// The caller is responsible for releasing the schema if NANOARROW_OK is returned.
///
/// Returns EINVAL if the decoder did not just decode a schema message or
/// NANOARROW_OK otherwise.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderDecodeSchema(struct ArrowIpcDecoder* decoder,
                                                         struct ArrowSchema* out,
                                                         struct ArrowError* error);

/// \brief Set the ArrowSchema used to decode future record batch messages
///
/// Prepares the decoder for future record batch messages
/// of this type. The decoder takes ownership of schema if NANOARROW_OK is returned.
/// Note that you must call this explicitly after decoding a
/// Schema message (i.e., the decoder does not assume that the last-decoded
/// schema message applies to future record batch messages).
///
/// Returns EINVAL if schema validation fails or NANOARROW_OK otherwise.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderSetSchema(struct ArrowIpcDecoder* decoder,
                                                      struct ArrowSchema* schema,
                                                      struct ArrowError* error);

/// \brief Set the endianness used to decode future record batch messages
///
/// Prepares the decoder for future record batch messages with the specified
/// endianness. Note that you must call this explicitly after decoding a
/// Schema message (i.e., the decoder does not assume that the last-decoded
/// schema message applies to future record batch messages).
///
/// Returns NANOARROW_OK on success.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderSetEndianness(
    struct ArrowIpcDecoder* decoder, enum ArrowIpcEndianness endianness);

/// \brief Decode an ArrowArrayView
///
/// After a successful call to ArrowIpcDecoderDecodeHeader(), deserialize the content
/// of body into an internally-managed ArrowArrayView and return it. Note that field index
/// does not equate to column index if any columns contain nested types. Use a value of -1
/// to decode the entire array into a struct. The pointed-to ArrowArrayView is owned by
/// the ArrowIpcDecoder and must not be released.
///
/// For streams that match system endianness and do not use compression, this operation
/// will not perform any heap allocations; however, the buffers referred to by the
/// returned ArrowArrayView are only valid as long as the buffer referred to by body stays
/// valid.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderDecodeArrayView(
    struct ArrowIpcDecoder* decoder, struct ArrowBufferView body, int64_t i,
    struct ArrowArrayView** out, struct ArrowError* error);

/// \brief Decode an ArrowArray
///
/// After a successful call to ArrowIpcDecoderDecodeHeader(), assemble an ArrowArray given
/// a message body and a field index. Note that field index does not equate to column
/// index if any columns contain nested types. Use a value of -1 to decode the entire
/// array into a struct. The caller is responsible for releasing the array if
/// NANOARROW_OK is returned.
///
/// Returns EINVAL if the decoder did not just decode a record batch message, ENOTSUP
/// if the message uses features not supported by this library, or or NANOARROW_OK
/// otherwise.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderDecodeArray(
    struct ArrowIpcDecoder* decoder, struct ArrowBufferView body, int64_t i,
    struct ArrowArray* out, enum ArrowValidationLevel validation_level,
    struct ArrowError* error);

/// \brief Decode an ArrowArray from an owned buffer
///
/// This implementation takes advantage of the fact that it can avoid copying individual
/// buffers. In all cases the caller must ArrowIpcSharedBufferReset() body after one or
/// more calls to ArrowIpcDecoderDecodeArrayFromShared(). If
/// ArrowIpcSharedBufferIsThreadSafe() returns 0, out must not be released by another
/// thread.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderDecodeArrayFromShared(
    struct ArrowIpcDecoder* decoder, struct ArrowIpcSharedBuffer* shared, int64_t i,
    struct ArrowArray* out, enum ArrowValidationLevel validation_level,
    struct ArrowError* error);

/// \brief An user-extensible input data source
struct ArrowIpcInputStream {
  /// \brief Read up to buf_size_bytes from stream into buf
  ///
  /// The actual number of bytes read is placed in the value pointed to by
  /// size_read_out. Returns NANOARROW_OK on success.
  ArrowErrorCode (*read)(struct ArrowIpcInputStream* stream, uint8_t* buf,
                         int64_t buf_size_bytes, int64_t* size_read_out,
                         struct ArrowError* error);

  /// \brief Release the stream and any resources it may be holding
  ///
  /// Release callback implementations must set the release member to NULL.
  /// Callers must check that the release callback is not NULL before calling
  /// read() or release().
  void (*release)(struct ArrowIpcInputStream* stream);

  /// \brief Private implementation-defined data
  void* private_data;
};

/// \brief Transfer ownership of an ArrowIpcInputStream
NANOARROW_DLL void ArrowIpcInputStreamMove(struct ArrowIpcInputStream* src,
                                           struct ArrowIpcInputStream* dst);

/// \brief Create an input stream from an ArrowBuffer
///
/// The stream takes ownership of the buffer and reads bytes from it.
NANOARROW_DLL ArrowErrorCode ArrowIpcInputStreamInitBuffer(
    struct ArrowIpcInputStream* stream, struct ArrowBuffer* input);

/// \brief Create an input stream from a C FILE* pointer
///
/// Note that the ArrowIpcInputStream has no mechanism to communicate an error
/// if file_ptr fails to close. If this behaviour is needed, pass false to
/// close_on_release and handle closing the file independently from stream.
NANOARROW_DLL ArrowErrorCode ArrowIpcInputStreamInitFile(
    struct ArrowIpcInputStream* stream, void* file_ptr, int close_on_release);

/// \brief Options for ArrowIpcArrayStreamReaderInit()
struct ArrowIpcArrayStreamReaderOptions {
  /// \brief The field index to extract.
  ///
  /// Defaults to -1 (i.e., read all fields). Note that this field index refers to
  /// the flattened tree of children and not necessarily the column index.
  int64_t field_index;

  /// \brief Set to a non-zero value to share the message body buffer among decoded arrays
  ///
  /// Sharing buffers is a good choice when (1) using memory-mapped IO
  /// (since unreferenced portions of the file are often not loaded into memory) or
  /// (2) if all data from all columns are about to be referenced anyway. When loading
  /// a single field there is probably no advantage to using shared buffers.
  /// Defaults to the value of ArrowIpcSharedBufferIsThreadSafe().
  int use_shared_buffers;
};

/// \brief Initialize an ArrowArrayStream from an input stream of bytes
///
/// The stream of bytes must begin with a Schema message and be followed by
/// zero or more RecordBatch messages as described in the Arrow IPC stream
/// format specification. Returns NANOARROW_OK on success. If NANOARROW_OK
/// is returned, the ArrowArrayStream takes ownership of input_stream and
/// the caller is responsible for releasing out.
NANOARROW_DLL ArrowErrorCode ArrowIpcArrayStreamReaderInit(
    struct ArrowArrayStream* out, struct ArrowIpcInputStream* input_stream,
    struct ArrowIpcArrayStreamReaderOptions* options);

/// \brief Encoder for Arrow IPC messages
///
/// This structure is intended to be allocated by the caller,
/// initialized using ArrowIpcEncoderInit(), and released with
/// ArrowIpcEncoderReset().
struct ArrowIpcEncoder {
  /// \brief Private resources managed by this library
  void* private_data;
};

/// \brief Initialize an encoder
///
/// If NANOARROW_OK is returned, the caller must call ArrowIpcEncoderReset()
/// to release resources allocated by this function.
NANOARROW_DLL ArrowErrorCode ArrowIpcEncoderInit(struct ArrowIpcEncoder* encoder);

/// \brief Release all resources attached to an encoder
NANOARROW_DLL void ArrowIpcEncoderReset(struct ArrowIpcEncoder* encoder);

/// \brief Finalize the most recently encoded message into a buffer
///
/// If specified, the message will be encapsulated (prefixed with the continuation
/// marker and the header size and 0-padded to a multiple of 8 bytes).
///
/// The bytes of the encoded message will be appended to the provided buffer.
NANOARROW_DLL ArrowErrorCode ArrowIpcEncoderFinalizeBuffer(
    struct ArrowIpcEncoder* encoder, char encapsulate, struct ArrowBuffer* out);

/// \brief Encode an ArrowSchema
///
/// Returns ENOMEM if allocation fails, NANOARROW_OK otherwise.
NANOARROW_DLL ArrowErrorCode ArrowIpcEncoderEncodeSchema(struct ArrowIpcEncoder* encoder,
                                                         const struct ArrowSchema* schema,
                                                         struct ArrowError* error);

/// \brief Encode a struct typed ArrayView to a flatbuffer RecordBatch, embedded in a
/// Message.
///
/// Body buffers are concatenated into a contiguous, padded body_buffer.
///
/// Returns ENOMEM if allocation fails, NANOARROW_OK otherwise.
NANOARROW_DLL ArrowErrorCode ArrowIpcEncoderEncodeSimpleRecordBatch(
    struct ArrowIpcEncoder* encoder, const struct ArrowArrayView* array_view,
    struct ArrowBuffer* body_buffer, struct ArrowError* error);

/// \brief An user-extensible output data sink
struct ArrowIpcOutputStream {
  /// \brief Write up to buf_size_bytes from stream into buf
  ///
  /// The actual number of bytes written is placed in the value pointed to by
  /// size_read_out. Returns NANOARROW_OK on success.
  ArrowErrorCode (*write)(struct ArrowIpcOutputStream* stream, const void* buf,
                          int64_t buf_size_bytes, int64_t* size_written_out,
                          struct ArrowError* error);

  /// \brief Release the stream and any resources it may be holding
  ///
  /// Release callback implementations must set the release member to NULL.
  /// Callers must check that the release callback is not NULL before calling
  /// read() or release().
  void (*release)(struct ArrowIpcOutputStream* stream);

  /// \brief Private implementation-defined data
  void* private_data;
};

/// \brief Transfer ownership of an ArrowIpcOutputStream
NANOARROW_DLL void ArrowIpcOutputStreamMove(struct ArrowIpcOutputStream* src,
                                            struct ArrowIpcOutputStream* dst);

/// \brief Create an output stream from an ArrowBuffer
///
/// All bytes witten to the stream will be appended to the buffer.
/// The stream does not take ownership of the buffer.
NANOARROW_DLL ArrowErrorCode ArrowIpcOutputStreamInitBuffer(
    struct ArrowIpcOutputStream* stream, struct ArrowBuffer* output);

/// \brief Create an output stream from a C FILE* pointer
///
/// Note that the ArrowIpcOutputStream has no mechanism to communicate an error
/// if file_ptr fails to close. If this behaviour is needed, pass false to
/// close_on_release and handle closing the file independently from stream.
NANOARROW_DLL ArrowErrorCode ArrowIpcOutputStreamInitFile(
    struct ArrowIpcOutputStream* stream, void* file_ptr, int close_on_release);

/// \brief Write to a stream, trying again until all are written or the stream errors.
NANOARROW_DLL ArrowErrorCode
ArrowIpcOutputStreamWrite(struct ArrowIpcOutputStream* stream,
                          struct ArrowBufferView data, struct ArrowError* error);

/// \brief A stream writer which encodes Schemas and ArrowArrays into an IPC byte stream
///
/// This structure is intended to be allocated by the caller,
/// initialized using ArrowIpcWriterInit(), and released with
/// ArrowIpcWriterReset().
struct ArrowIpcWriter {
  /// \brief Private resources managed by this library
  void* private_data;
};

/// \brief Initialize an output stream of bytes from an ArrowArrayStream
///
/// Returns NANOARROW_OK on success. If NANOARROW_OK is returned the writer
/// takes ownership of the output byte stream, and the caller is
/// responsible for releasing the writer by calling ArrowIpcWriterReset().
NANOARROW_DLL ArrowErrorCode ArrowIpcWriterInit(
    struct ArrowIpcWriter* writer, struct ArrowIpcOutputStream* output_stream);

/// \brief Release all resources attached to a writer
NANOARROW_DLL void ArrowIpcWriterReset(struct ArrowIpcWriter* writer);

/// \brief Write a schema to the output byte stream
///
/// Errors are propagated from the underlying encoder and output byte stream.
NANOARROW_DLL ArrowErrorCode ArrowIpcWriterWriteSchema(struct ArrowIpcWriter* writer,
                                                       const struct ArrowSchema* in,
                                                       struct ArrowError* error);

/// \brief Write an array view to the output byte stream
///
/// The array view may be NULL, in which case an EOS will be written.
/// The writer does not check that a schema was already written.
///
/// Errors are propagated from the underlying encoder and output byte stream,
NANOARROW_DLL ArrowErrorCode ArrowIpcWriterWriteArrayView(struct ArrowIpcWriter* writer,
                                                          const struct ArrowArrayView* in,
                                                          struct ArrowError* error);

/// \brief Write an entire stream (including EOS) to the output byte stream
///
/// Errors are propagated from the underlying encoder, array stream, and output byte
/// stream.
NANOARROW_DLL ArrowErrorCode ArrowIpcWriterWriteArrayStream(struct ArrowIpcWriter* writer,
                                                            struct ArrowArrayStream* in,
                                                            struct ArrowError* error);

/// \brief Start writing an IPC file
///
/// Writes the Arrow IPC magic and sets the writer up to track written blocks.
ArrowErrorCode ArrowIpcWriterStartFile(struct ArrowIpcWriter* writer,
                                       struct ArrowError* error);

/// \brief Finish writing an IPC file
///
/// Writes the IPC file's footer, footer size, and ending magic.
NANOARROW_DLL ArrowErrorCode ArrowIpcWriterFinalizeFile(struct ArrowIpcWriter* writer,
                                                        struct ArrowError* error);
/// @}

// Internal APIs:

/// \brief Represents a byte range in an IPC file.
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
struct ArrowIpcFileBlock {
  /// \brief offset relative to the first byte of the file.
  int64_t offset;
  /// \brief length of encapsulated metadata Message (including padding)
  int32_t metadata_length;
  /// \brief length of contiguous body buffers (including padding)
  int64_t body_length;
};

/// \brief A footer for use in an IPC file
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
///
/// This structure is intended to be allocated by the caller, initialized using
/// ArrowIpcFooterInit(), and released with ArrowIpcFooterReset().
struct ArrowIpcFooter {
  /// \brief the Footer's embedded Schema
  struct ArrowSchema schema;
  /// \brief all blocks containing RecordBatch Messages
  struct ArrowBuffer record_batch_blocks;
};

/// \brief Initialize a footer
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
NANOARROW_DLL void ArrowIpcFooterInit(struct ArrowIpcFooter* footer);

/// \brief Release all resources attached to an footer
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
NANOARROW_DLL void ArrowIpcFooterReset(struct ArrowIpcFooter* footer);

/// \brief Encode a footer for use in an IPC file
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
///
/// Returns ENOMEM if allocation fails, NANOARROW_OK otherwise.
NANOARROW_DLL ArrowErrorCode ArrowIpcEncoderEncodeFooter(
    struct ArrowIpcEncoder* encoder, const struct ArrowIpcFooter* footer,
    struct ArrowError* error);

/// \brief Peek at a footer
///
/// The last 10 bytes of an Arrow IPC file are the footer size as a little-endian
/// 32-bit integer followed by the ARROW1 magic. ArrowIpcDecoderPeekFooter() reads
/// these bytes and returns ESPIPE if there are not enough remaining bytes in data
/// to read the entire footer, EINVAL if the last 10 bytes are not valid,
/// or NANOARROW_OK otherwise.
///
/// The footer size will be stored in decoder.header_size_bytes.
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderPeekFooter(struct ArrowIpcDecoder* decoder,
                                                       struct ArrowBufferView data,
                                                       struct ArrowError* error);

/// \brief Verify a footer
///
/// Runs ArrowIpcDecoderPeekFooter() to ensure data is sufficiently large but additionally
/// runs flatbuffer verification to ensure that decoding the data will not access
/// memory outside of the buffer specified by data. ArrowIpcDecoderVerifyFooter() will
/// also set decoder.header_size_bytes and decoder.metadata_version.
///
/// Returns as ArrowIpcDecoderPeekFooter() and additionally will
/// return EINVAL if flatbuffer verification fails.
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderVerifyFooter(struct ArrowIpcDecoder* decoder,
                                                         struct ArrowBufferView data,
                                                         struct ArrowError* error);

/// \brief Decode a footer
///
/// Runs ArrowIpcDecoderPeekFooter() to ensure data is sufficiently large and decodes
/// the content of the footer. decoder.footer will be set for access to the file's
/// schema and record batches. In almost all cases this should be preceded by a call to
/// ArrowIpcDecoderVerifyFooter() to ensure decoding does not access data outside of the
/// specified buffer.
///
/// Returns EINVAL if the content of the footer cannot be decoded or ENOTSUP if the
/// content of the footer uses features not supported by this library.
///
/// \warning This API is currently only public for use in integration testing;
///          use at your own risk.
NANOARROW_DLL ArrowErrorCode ArrowIpcDecoderDecodeFooter(struct ArrowIpcDecoder* decoder,
                                                         struct ArrowBufferView data,
                                                         struct ArrowError* error);

#ifdef __cplusplus
}
#endif

#endif
