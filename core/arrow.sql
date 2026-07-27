-- Query a heap produced by:
--
--     wisp arrow wisp.arrow
--     duckdb < core/arrow.sql

LOAD nanoarrow;

CREATE OR REPLACE VIEW heap AS
    SELECT * FROM read_arrow('wisp.arrow');

CREATE OR REPLACE VIEW bytes AS
    SELECT
        i::UINTEGER AS idx,
        (
            '0x' ||
            substr(hex(heap.bytes), 2 * i + 1, 2)
        )::UTINYINT AS byte
    FROM heap,
         range(0, octet_length(heap.bytes)) AS byte_range(i);

CREATE OR REPLACE VIEW words AS
    SELECT
        (ordinal - 1)::UINTEGER AS idx,
        word
    FROM heap,
         UNNEST(heap.words) WITH ORDINALITY
             AS item(word, ordinal);

CREATE OR REPLACE VIEW duo AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.duo) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW sym AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.sym) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW fun AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.fun) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW mac AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.mac) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW v08 AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.v08) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW v32 AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.v32) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW pkg AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.pkg) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW run AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.run) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW ktx AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.ktx) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW ext AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.ext) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE VIEW pins AS
    SELECT
        (ordinal - 1)::UINTEGER AS row_idx,
        object.*
    FROM heap,
         UNNEST(heap.pins) WITH ORDINALITY
             AS item(object, ordinal);

CREATE OR REPLACE MACRO word_tag(word) AS
    ((word::UBIGINT >> 27)::UTINYINT);

CREATE OR REPLACE MACRO word_era(word) AS
    ((word::UBIGINT & 1)::UTINYINT);

CREATE OR REPLACE MACRO word_idx(word) AS
    (((word::UBIGINT >> 1) & 67108863)::UINTEGER);

CREATE OR REPLACE MACRO word_tag_name(word) AS
    CASE word_tag(word)
        WHEN 0 THEN 'int'
        WHEN 17 THEN 'sys'
        WHEN 18 THEN 'chr'
        WHEN 19 THEN 'jet'
        WHEN 21 THEN 'duo'
        WHEN 22 THEN 'sym'
        WHEN 23 THEN 'fun'
        WHEN 24 THEN 'mac'
        WHEN 25 THEN 'v32'
        WHEN 26 THEN 'v08'
        WHEN 27 THEN 'pkg'
        WHEN 28 THEN 'run'
        WHEN 29 THEN 'ktx'
        WHEN 30 THEN 'ext'
        WHEN 31 THEN 'pin'
        ELSE 'unknown'
    END;

CREATE OR REPLACE VIEW string AS
    SELECT
        v08.row_idx,
        decode(
            from_hex(
                substr(
                    hex(heap.bytes),
                    2 * v08.idx + 1,
                    2 * v08.len
                )
            )
        ) AS value
    FROM v08
    CROSS JOIN heap;

CREATE OR REPLACE VIEW symbol AS
    SELECT
        sym.*,
        string.value AS name,
        word_tag_name(sym.val) AS value_type,
        word_tag_name(sym.fun) AS function_type
    FROM sym
    LEFT JOIN string
        ON string.row_idx = word_idx(sym.str);
