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

const word = @import("./word.zig");
const heap = @import("./heap.zig");

// Re-export all public declarations from word.zig
pub const Tag = word.Tag;
pub const pointerTags = word.pointerTags;
pub const Era = word.Era;
pub const Ptr = word.Ptr;
pub const Imm = word.Imm;
pub const Word = word.Word;
pub const ref = word.ref;
pub const nil = word.nil;
pub const t = word.t;
pub const nah = word.nah;
pub const zap = word.zap;
pub const top = word.top;
pub const tagOf = word.tagOf;

// Re-export all public declarations from heap.zig
pub const ColEnum = heap.ColEnum;
pub const Kwd = heap.Kwd;
pub const Orb = heap.Orb;
pub const V08 = heap.V08;
pub const V32 = heap.V32;
pub const Oof = heap.Oof;
pub const badPointerTag = heap.badPointerTag;
pub const Row = heap.Row;
pub const Col = heap.Col;
pub const Tab = heap.Tab;
pub const Vat = heap.Vat;
pub const MsgTag = heap.MsgTag;
pub const CommonStrings = heap.CommonStrings;
pub const Heap = heap.Heap;
pub const list = heap.list;
pub const length = heap.length;
pub const listItemsIntoSlice = heap.listItemsIntoSlice;

const root = @import("root");

pub const browser: bool = @hasDecl(root, "wisp_browser");
