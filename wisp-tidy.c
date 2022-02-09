/*
 * This file is part of Wisp.
 *
 * Wisp is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * Foobar is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public
 * License for more details.

 * You should have received a copy of the GNU Affero General Public
 * License along with Wisp. If not, see <https://www.gnu.org/licenses/>.
 */

#include "wisp.h"

#define WISP_DEBUG_GC 0

int
wisp_instance_size (wisp_word_t *data)
{
  wisp_word_t length = *data >> 8;
  return wisp_align ((1 + length) * WISP_WORD_SIZE);
}

int
wisp_string_size (wisp_word_t *data)
{
  wisp_word_t length = *data >> 8;
  return wisp_align (WISP_WORD_SIZE + length + 1);
}

int
wisp_object_size (wisp_word_t *data)
{
  switch (data[0] & 0xff)
    {
    case WISP_WIDETAG_INSTANCE:
    case WISP_WIDETAG_SYMBOL:
      {
        return wisp_instance_size (data);
      }

    case WISP_WIDETAG_STRING:
      {
        return wisp_string_size (data);
      }

    default:
      return 2 * WISP_WORD_SIZE;
    }

}

#define WISP_POINTS_TO_STATIC_SPACE(ptr) \
  (((ptr) & ~7) < WISP_STATIC_SPACE_SIZE)

wisp_word_t
wisp_gc_copy_to_new_heap (wisp_word_t ptr)
{
  if (!WISP_IS_PTR (ptr) || WISP_POINTS_TO_STATIC_SPACE (ptr))
    {
      return ptr;
    }

#if WISP_DEBUG_GC
  WISP_DEBUG ("copying ");
  wisp_dump (stderr, ptr);
  WISP_DEBUG ("\n");
#endif

  wisp_word_t *data = wisp_deref (ptr);
  wisp_word_t lowtag = ptr & 7;

  if (WISP_IS_PTR (data[0]) && (data[0] & ~7) >= wisp_new_heap && (data[0] & ~7) <= (wisp_new_heap + heap_size))
    {
#if WISP_DEBUG_GC
      WISP_DEBUG ("   already copied [0x%x]\n", data[0] & ~7);
#endif

      assert ((data[0] & 7) == (ptr & 7));
      return data[0];
    }

  int n = wisp_object_size (data);
  int dst = wisp_new_heap + wisp_heap_used;

  wisp_heap_used += n;

  assert (wisp_heap_used < heap_size);

#if WISP_DEBUG_GC
  WISP_DEBUG ("   [0x%x] to [0x%x]\n", dst, dst + n);
#endif

  memcpy (wisp_heap_base + dst, data, n);

  data[0] = dst | lowtag;

  return dst | lowtag;
}

void
wisp_scavenge (void)
{
  wisp_word_t *header =
    wisp_deref (wisp_new_heap_scan | WISP_LOWTAG_OTHER_PTR);

#if WISP_DEBUG_GC
  WISP_DEBUG ("scavenging [0x%x] ", wisp_new_heap_scan);
  wisp_dump (stderr, *header);
  WISP_DEBUG ("\n");
#endif

  if (*header == 0)
    {
      wisp_new_heap_scan += WISP_WORD_SIZE;
      return;
    }

  switch (*header & 0xff)
    {
    case WISP_WIDETAG_INSTANCE:
    case WISP_WIDETAG_SYMBOL:
      {
        int n = *header >> 8;

        for (int i = 0; i < n + 1; i++)
          {
#if WISP_DEBUG_GC
            WISP_DEBUG ("#%d ", i);
#endif
            header[i] = wisp_gc_copy_to_new_heap (header[i]);
          }

        wisp_new_heap_scan += wisp_instance_size (header);

        break;
      }

    case WISP_WIDETAG_STRING:
      {
        wisp_new_heap_scan += wisp_string_size (header);
        break;
      }

    default:
      {
        int n = 2;

        for (int i = 0; i < n; i++)
          {
#if WISP_DEBUG_GC
            WISP_DEBUG ("@%d ", i);
#endif
            header[i] = wisp_gc_copy_to_new_heap (header[i]);
            wisp_new_heap_scan += WISP_WORD_SIZE;
          }

        break;
      }
    }
}

void
wisp_flip (void)
{
  int old_heap = wisp_old_heap;
  int new_heap = wisp_new_heap;

  wisp_new_heap = old_heap;
  wisp_old_heap = new_heap;

  wisp_heap_used = WISP_STATIC_SPACE_SIZE;
  wisp_new_heap_scan = wisp_new_heap;

  wisp_heap = wisp_heap_base;

  /* memset (wisp_heap_base + wisp_new_heap + WISP_STATIC_SPACE_SIZE, */
  /*         0, */
  /*         heap_size - WISP_STATIC_SPACE_SIZE); */
}

WISP_EXPORT
void
wisp_tidy (void)
{
  WISP_DEBUG ("\n* collecting garbage\n");

  int old_size = wisp_heap_used;

  wisp_flip ();
  for (int i = 0; i < wisp_cache_size; i++)
    {
      /* WISP_DEBUG ("base gc root %d: ", i); */
      /* wisp_dump (stderr, wisp_cache[i]); */
      /* WISP_DEBUG ("\n"); */
      wisp_cache[i] = wisp_gc_copy_to_new_heap (wisp_cache[i]);
    }

  if (wisp_machine)
    {
      /* WISP_DEBUG ("gc root term: "); */
      /* wisp_dump (stderr, wisp_machine->term); */
      /* WISP_DEBUG ("\n"); */
      wisp_machine->term = wisp_gc_copy_to_new_heap (wisp_machine->term);
      /* WISP_DEBUG ("gc new root term: "); */
      /* wisp_dump (stderr, wisp_machine->term); */
      /* WISP_DEBUG ("\n"); */

      /* WISP_DEBUG ("gc root scopes: "); */
      /* wisp_dump (stderr, wisp_machine->scopes); */
      /* WISP_DEBUG ("\n"); */
      wisp_machine->scopes = wisp_gc_copy_to_new_heap (wisp_machine->scopes);

      /* WISP_DEBUG ("gc root plan: "); */
      /* wisp_dump (stderr, wisp_machine->plan); */
      /* WISP_DEBUG ("\n"); */
      wisp_machine->plan = wisp_gc_copy_to_new_heap (wisp_machine->plan);
    }

  /* WISP_CACHE (WISP) = wisp_gc_copy_to_new_heap (WISP_CACHE (WISP)); */

  while (wisp_new_heap_scan < wisp_new_heap + wisp_heap_used)
    wisp_scavenge ();

  WISP_DEBUG ("old heap %.2f KiB, new heap %.2f KiB (%d B)\n",
              old_size / 1024.0,
              wisp_heap_used / 1024.0,
              wisp_heap_used - old_size);

  memset (wisp_heap_base + wisp_old_heap + WISP_STATIC_SPACE_SIZE,
          0,
          heap_size - WISP_STATIC_SPACE_SIZE);
}
