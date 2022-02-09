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

WISP_EXPORT
void
wisp_dump_stdout (wisp_word_t word)
{
  wisp_dump (stdout, word);
  printf ("\n");
}

void
wisp_dump (FILE *f, wisp_word_t word)
{
  wisp_word_t widetag = WISP_WIDETAG (word);

  if (WISP_IS_FIXNUM (word))
    fprintf (f, "%d", word >> 2);

  else if (word == NIL)
    fprintf (f, "NIL");

  else if (wisp_is_quote (word))
    {
      wisp_word_t quoted = wisp_car (wisp_cdr (word));
      fprintf (f, "'");
      wisp_dump (f, quoted);
    }

  else if (WISP_IS_LIST_PTR (word))
    {
      fprintf (f, "(");

      while (word != NIL)
        {
          wisp_word_t *cons = wisp_deref (word);
          wisp_word_t  car = cons[0];
          wisp_word_t  cdr = cons[1];

          wisp_dump (f, car);

          if (cdr == NIL)
            {
              break;
            }
          else if (WISP_IS_LIST_PTR (cdr))
            {
              fprintf (f, " ");
              word = cdr;
            }
          else
            {
              fprintf (f, " . ");
              wisp_dump (f, cdr);
              break;
            }
        }

      fprintf (f, ")");
    }

  else if (WISP_IS_STRUCT_PTR (word))
    {
      wisp_word_t *header = wisp_deref (word);
      if ((header[0] & 0xff) == WISP_WIDETAG_INSTANCE)
        {
          fprintf (f, "«struct ");

          wisp_word_t *struct_header = wisp_deref (word);

          wisp_dump (f, struct_header[1]);
          fprintf (f, " 0x%X»", word);
        }
      else
        {
          fprintf (f, "«moved struct ");

          wisp_word_t *struct_header = wisp_deref (word);

          wisp_dump (f, struct_header[1]);
          fprintf (f, " 0x%X»", word);
        }
    }

  else if (WISP_IS_OTHER_PTR (word))
    {
      wisp_word_t *header = wisp_deref (word);
      if (header[0] == WISP_SYMBOL_HEADER)
        {
          wisp_word_t *string_header = wisp_deref (header[4]);
          fprintf (f, "%s", wisp_string_buffer (string_header));
        }
      else if (WISP_WIDETAG (header[0]) == WISP_WIDETAG_STRING)
        {
          fprintf (f, "\"%s\"", wisp_string_buffer (header));
        }
      else if (WISP_IS_PTR (header[0]))
        {
          fprintf (f, "{heart 0x%x}", header[0] & ~7);
        }
      else
        {
          WISP_DEBUG ("{OTHER-PTR 0x%x tag %x}", word, header[0] & 0xff);
          /* wisp_not_implemented (); */
        }
    }

  else if (widetag == WISP_WIDETAG_BUILTIN)
    fprintf (f, "%%%d", word >> 8);

  else if (widetag == WISP_WIDETAG_INSTANCE)
    fprintf (f, "{instance header}");

  else if (widetag == WISP_WIDETAG_STRING)
    fprintf (f, "{string header}");

  else if (widetag == WISP_WIDETAG_SYMBOL)
    fprintf (f, "{symbol header 0x%x}", word);

  else
    fprintf (f, "{unknown 0x%x}", word);
}
