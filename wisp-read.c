/*
 * This file is part of Wisp.
 *
 * Wisp is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * Wisp is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
 * Public License for more details.

 * You should have received a copy of the GNU Affero General Public
 * License along with Wisp. If not, see <https://www.gnu.org/licenses/>.
 */

#include "wisp.h"

wisp_word_t
wisp_read_list (const char **stream)
{
  while (isspace (**stream))
    ++*stream;

  char c = **stream;

  if (c == ')')
    {
      ++*stream;
      return NIL;
    }

  else if (c == '.')
    {
      ++*stream;
      wisp_word_t cdr = wisp_read (stream);

      while (isspace (**stream))
        ++*stream;

      if (**stream != ')')
        wisp_crash ("expected end of list");

      ++*stream;

      return cdr;
    }
  else
    {
      wisp_word_t car = wisp_read (stream);
      wisp_word_t cdr = wisp_read_list (stream);
      return wisp_cons (car, cdr);
    }
}

bool
wisp_is_symbol_char (char c)
{
  return isalpha (c)
    || c == '+'
    || c == '-'
    || c == '='
    || c == '/'
    || c == '^'
    || c == '*'
    || c == '%'
    || c == '$'
    || c == '@';
}

wisp_word_t
wisp_read_symbol (const char **stream)
{
  const char *after = *stream + 1;

  while (wisp_is_symbol_char (*after))
    ++after;

  int length = ++after - *stream - 1;

  wisp_word_t name =
    wisp_string_n (*stream, length);

  char *data =
    wisp_string_buffer (wisp_deref (name));

  for (int i = 0; i < length; i++)
    data[i] = toupper (data[i]);

  *stream = after - 1;

  return wisp_intern_symbol (name, WISP_CACHE (WISP));
}

wisp_word_t
wisp_read_fixnum (const char **stream)
{
  const char *after = *stream + 1;

  while (isdigit (*after++));

  int length = after - *stream - 1;

  if (length > 10)
    wisp_not_implemented ();

  char digits[length];

  for (int i = 0; i < length; i++)
    digits[i] = (*stream)[i];

  digits[length] = 0;

  *stream = after - 1;

  return wisp_fixnum (atoi (digits));
}

wisp_word_t
wisp_read_string (const char **stream)
{
  int length = 0;

  const char *x = *stream;
  while (*x && *x != '"')
    ++x;

  length = x - *stream;

  wisp_word_t string =
    wisp_string_n (*stream, length);

  if (*x == '"')
    {
      *stream = x + 1;
      return string;
    }
  else
    wisp_crash ("string never ended");
}

wisp_word_t
wisp_read (const char **stream)
{
  while (isspace (**stream))
    ++*stream;

  char c = **stream;

  if (c == 0)
    wisp_not_implemented ();

  if (c == '(')
    {
      ++*stream;
      return wisp_read_list (stream);
    }

  if (wisp_is_symbol_char (c))
    return wisp_read_symbol (stream);

  if (isdigit (c))
    return wisp_read_fixnum (stream);

  if (c == '\'')
    {
      ++*stream;
      return wisp_cons (WISP_CACHE (QUOTE),
                        wisp_cons (wisp_read (stream), NIL));
    }

  if (c == '"')
    {
      ++*stream;
      return wisp_read_string (stream);
    }

  wisp_not_implemented ();
}

WISP_EXPORT
wisp_word_t
wisp_read_from_string (const char *string)
{
  return wisp_read (&string);
}
