var __create = Object.create;
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getProtoOf = Object.getPrototypeOf;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __commonJS = (cb, mod) => function __require() {
  return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target, mod));

// node_modules/incremental-dom/dist/incremental-dom-cjs.js
var require_incremental_dom_cjs = __commonJS({
  "node_modules/incremental-dom/dist/incremental-dom-cjs.js"(exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var keyAttributeName = "key";
    function getKeyAttributeName() {
      return keyAttributeName;
    }
    function setKeyAttributeName(name) {
      keyAttributeName = name;
    }
    var inAttributes = false;
    var inSkip = false;
    var inPatch = false;
    function assert(val) {
      if (!val) {
        throw new Error("Expected value to be defined");
      }
      return val;
    }
    function assertInPatch(functionName) {
      if (!inPatch) {
        throw new Error("Cannot call " + functionName + "() unless in patch.");
      }
    }
    function assertNoUnclosedTags(openElement, root) {
      if (openElement === root) {
        return;
      }
      let currentElement2 = openElement;
      const openTags = [];
      while (currentElement2 && currentElement2 !== root) {
        openTags.push(currentElement2.nodeName.toLowerCase());
        currentElement2 = currentElement2.parentNode;
      }
      throw new Error("One or more tags were not closed:\n" + openTags.join("\n"));
    }
    function assertPatchOuterHasParentNode(parent) {
      if (!parent) {
        console.warn("patchOuter requires the node have a parent if there is a key.");
      }
    }
    function assertNotInAttributes(functionName) {
      if (inAttributes) {
        throw new Error(functionName + "() can not be called between elementOpenStart() and elementOpenEnd().");
      }
    }
    function assertNotInSkip(functionName) {
      if (inSkip) {
        throw new Error(functionName + "() may not be called inside an element that has called skip().");
      }
    }
    function assertInAttributes(functionName) {
      if (!inAttributes) {
        throw new Error(functionName + "() can only be called after calling elementOpenStart().");
      }
    }
    function assertVirtualAttributesClosed() {
      if (inAttributes) {
        throw new Error("elementOpenEnd() must be called after calling elementOpenStart().");
      }
    }
    function assertCloseMatchesOpenTag(currentNameOrCtor, nameOrCtor) {
      if (currentNameOrCtor !== nameOrCtor) {
        throw new Error('Received a call to close "' + nameOrCtor + '" but "' + currentNameOrCtor + '" was open.');
      }
    }
    function assertNoChildrenDeclaredYet(functionName, previousNode) {
      if (previousNode !== null) {
        throw new Error(functionName + "() must come before any child declarations inside the current element.");
      }
    }
    function assertPatchElementNoExtras(maybeStartNode, maybeCurrentNode, expectedNextNode, expectedPrevNode) {
      const startNode = assert(maybeStartNode);
      const currentNode2 = assert(maybeCurrentNode);
      const wasUpdated = currentNode2.nextSibling === expectedNextNode && currentNode2.previousSibling === expectedPrevNode;
      const wasChanged = currentNode2.nextSibling === startNode.nextSibling && currentNode2.previousSibling === expectedPrevNode;
      const wasRemoved = currentNode2 === startNode;
      if (!wasUpdated && !wasChanged && !wasRemoved) {
        throw new Error("There must be exactly one top level call corresponding to the patched element.");
      }
    }
    function updatePatchContext(newContext) {
      inPatch = newContext != null;
    }
    function setInAttributes(value) {
      const previous = inAttributes;
      inAttributes = value;
      return previous;
    }
    function setInSkip(value) {
      const previous = inSkip;
      inSkip = value;
      return previous;
    }
    var hasOwnProperty = Object.prototype.hasOwnProperty;
    function Blank() {
    }
    Blank.prototype = /* @__PURE__ */ Object.create(null);
    function has(map, property) {
      return hasOwnProperty.call(map, property);
    }
    function createMap() {
      return new Blank();
    }
    function truncateArray(arr, length) {
      while (arr.length > length) {
        arr.pop();
      }
    }
    function createArray(initialAllocationSize) {
      const arr = new Array(initialAllocationSize);
      truncateArray(arr, 0);
      return arr;
    }
    var symbols = {
      default: "__default"
    };
    function getNamespace(name) {
      if (name.lastIndexOf("xml:", 0) === 0) {
        return "http://www.w3.org/XML/1998/namespace";
      }
      if (name.lastIndexOf("xlink:", 0) === 0) {
        return "http://www.w3.org/1999/xlink";
      }
      return null;
    }
    function applyAttr(el, name, value) {
      if (value == null) {
        el.removeAttribute(name);
      } else {
        const attrNS = getNamespace(name);
        if (attrNS) {
          el.setAttributeNS(attrNS, name, String(value));
        } else {
          el.setAttribute(name, String(value));
        }
      }
    }
    function applyProp(el, name, value) {
      el[name] = value;
    }
    function setStyleValue(style, prop, value) {
      if (prop.indexOf("-") >= 0) {
        style.setProperty(prop, value);
      } else {
        style[prop] = value;
      }
    }
    function applyStyle(el, name, style) {
      assert("style" in el);
      const elStyle = el.style;
      if (typeof style === "string") {
        elStyle.cssText = style;
      } else {
        elStyle.cssText = "";
        for (const prop in style) {
          if (has(style, prop)) {
            setStyleValue(elStyle, prop, style[prop]);
          }
        }
      }
    }
    function applyAttributeTyped(el, name, value) {
      const type = typeof value;
      if (type === "object" || type === "function") {
        applyProp(el, name, value);
      } else {
        applyAttr(el, name, value);
      }
    }
    var attributes = createMap();
    attributes[symbols.default] = applyAttributeTyped;
    attributes["style"] = applyStyle;
    function updateAttribute(el, name, value) {
      const mutator = attributes[name] || attributes[symbols.default];
      mutator(el, name, value);
    }
    var notifications = {
      nodesCreated: null,
      nodesDeleted: null
    };
    var Context = class {
      constructor() {
        this.created = [];
        this.deleted = [];
      }
      markCreated(node) {
        this.created.push(node);
      }
      markDeleted(node) {
        this.deleted.push(node);
      }
      notifyChanges() {
        if (notifications.nodesCreated && this.created.length > 0) {
          notifications.nodesCreated(this.created);
        }
        if (notifications.nodesDeleted && this.deleted.length > 0) {
          notifications.nodesDeleted(this.deleted);
        }
      }
    };
    function isDocumentRoot(node) {
      return node.nodeType === 11 || node.nodeType === 9;
    }
    function isElement(node) {
      return node.nodeType === 1;
    }
    function getAncestry(node, root) {
      const ancestry = [];
      let cur = node;
      while (cur !== root) {
        const n = assert(cur);
        ancestry.push(n);
        cur = n.parentNode;
      }
      return ancestry;
    }
    var getRootNode = typeof Node !== "undefined" && Node.prototype.getRootNode || function() {
      let cur = this;
      let prev = cur;
      while (cur) {
        prev = cur;
        cur = cur.parentNode;
      }
      return prev;
    };
    function getActiveElement(node) {
      const root = getRootNode.call(node);
      return isDocumentRoot(root) ? root.activeElement : null;
    }
    function getFocusedPath(node, root) {
      const activeElement = getActiveElement(node);
      if (!activeElement || !node.contains(activeElement)) {
        return [];
      }
      return getAncestry(activeElement, root);
    }
    function moveBefore(parentNode, node, referenceNode) {
      const insertReferenceNode = node.nextSibling;
      let cur = referenceNode;
      while (cur !== null && cur !== node) {
        const next = cur.nextSibling;
        parentNode.insertBefore(cur, insertReferenceNode);
        cur = next;
      }
    }
    var NodeData = class {
      constructor(nameOrCtor, key2, text2) {
        this._attrsArr = null;
        this.staticsApplied = false;
        this.nameOrCtor = nameOrCtor;
        this.key = key2;
        this.text = text2;
      }
      hasEmptyAttrsArr() {
        const attrs = this._attrsArr;
        return !attrs || !attrs.length;
      }
      getAttrsArr(length) {
        return this._attrsArr || (this._attrsArr = createArray(length));
      }
    };
    function initData(node, nameOrCtor, key2, text2) {
      const data = new NodeData(nameOrCtor, key2, text2);
      node["__incrementalDOMData"] = data;
      return data;
    }
    function isDataInitialized(node) {
      return Boolean(node["__incrementalDOMData"]);
    }
    function recordAttributes(node, data) {
      const attributes2 = node.attributes;
      const length = attributes2.length;
      if (!length) {
        return;
      }
      const attrsArr = data.getAttrsArr(length);
      for (let i = 0, j = 0; i < length; i += 1, j += 2) {
        const attr2 = attributes2[i];
        const name = attr2.name;
        const value = attr2.value;
        attrsArr[j] = name;
        attrsArr[j + 1] = value;
      }
    }
    function importSingleNode(node, fallbackKey) {
      if (node["__incrementalDOMData"]) {
        return node["__incrementalDOMData"];
      }
      const nodeName = isElement(node) ? node.localName : node.nodeName;
      const keyAttrName = getKeyAttributeName();
      const keyAttr = isElement(node) && keyAttrName != null ? node.getAttribute(keyAttrName) : null;
      const key2 = isElement(node) ? keyAttr || fallbackKey : null;
      const data = initData(node, nodeName, key2);
      if (isElement(node)) {
        recordAttributes(node, data);
      }
      return data;
    }
    function importNode(node) {
      importSingleNode(node);
      for (let child = node.firstChild; child; child = child.nextSibling) {
        importNode(child);
      }
    }
    function getData(node, fallbackKey) {
      return importSingleNode(node, fallbackKey);
    }
    function getKey(node) {
      assert(node["__incrementalDOMData"]);
      return getData(node).key;
    }
    function clearCache(node) {
      node["__incrementalDOMData"] = null;
      for (let child = node.firstChild; child; child = child.nextSibling) {
        clearCache(child);
      }
    }
    function getNamespaceForTag(tag, parent) {
      if (tag === "svg") {
        return "http://www.w3.org/2000/svg";
      }
      if (tag === "math") {
        return "http://www.w3.org/1998/Math/MathML";
      }
      if (parent == null) {
        return null;
      }
      if (getData(parent).nameOrCtor === "foreignObject") {
        return null;
      }
      return parent.namespaceURI;
    }
    function createElement(doc2, parent, nameOrCtor, key2) {
      let el;
      if (typeof nameOrCtor === "function") {
        el = new nameOrCtor();
      } else {
        const namespace = getNamespaceForTag(nameOrCtor, parent);
        if (namespace) {
          el = doc2.createElementNS(namespace, nameOrCtor);
        } else {
          el = doc2.createElement(nameOrCtor);
        }
      }
      initData(el, nameOrCtor, key2);
      return el;
    }
    function createText(doc2) {
      const node = doc2.createTextNode("");
      initData(node, "#text", null);
      return node;
    }
    function defaultMatchFn(matchNode, nameOrCtor, expectedNameOrCtor, key2, expectedKey) {
      return nameOrCtor == expectedNameOrCtor && key2 == expectedKey;
    }
    var context = null;
    var currentNode = null;
    var currentParent = null;
    var doc = null;
    var focusPath = [];
    var matchFn = defaultMatchFn;
    var argsBuilder = [];
    var attrsBuilder = [];
    function getArgsBuilder() {
      return argsBuilder;
    }
    function getAttrsBuilder() {
      return attrsBuilder;
    }
    function matches(matchNode, nameOrCtor, key2) {
      const data = getData(matchNode, key2);
      return matchFn(matchNode, nameOrCtor, data.nameOrCtor, key2, data.key);
    }
    function getMatchingNode(matchNode, nameOrCtor, key2) {
      if (!matchNode) {
        return null;
      }
      let cur = matchNode;
      do {
        if (matches(cur, nameOrCtor, key2)) {
          return cur;
        }
      } while (key2 && (cur = cur.nextSibling));
      return null;
    }
    function clearUnvisitedDOM(maybeParentNode, startNode, endNode) {
      const parentNode = maybeParentNode;
      let child = startNode;
      while (child !== endNode) {
        const next = child.nextSibling;
        parentNode.removeChild(child);
        context.markDeleted(child);
        child = next;
      }
    }
    function getNextNode() {
      if (currentNode) {
        return currentNode.nextSibling;
      } else {
        return currentParent.firstChild;
      }
    }
    function enterNode() {
      currentParent = currentNode;
      currentNode = null;
    }
    function exitNode() {
      clearUnvisitedDOM(currentParent, getNextNode(), null);
      currentNode = currentParent;
      currentParent = currentParent.parentNode;
    }
    function nextNode() {
      currentNode = getNextNode();
    }
    function createNode(nameOrCtor, key2) {
      let node;
      if (nameOrCtor === "#text") {
        node = createText(doc);
      } else {
        node = createElement(doc, currentParent, nameOrCtor, key2);
      }
      context.markCreated(node);
      return node;
    }
    function alignWithDOM(nameOrCtor, key2) {
      nextNode();
      const existingNode = getMatchingNode(currentNode, nameOrCtor, key2);
      const node = existingNode || createNode(nameOrCtor, key2);
      if (node === currentNode) {
        return;
      }
      if (focusPath.indexOf(node) >= 0) {
        moveBefore(currentParent, node, currentNode);
      } else {
        currentParent.insertBefore(node, currentNode);
      }
      currentNode = node;
    }
    function open(nameOrCtor, key2) {
      alignWithDOM(nameOrCtor, key2);
      enterNode();
      return currentParent;
    }
    function close() {
      {
        setInSkip(false);
      }
      exitNode();
      return currentNode;
    }
    function text() {
      alignWithDOM("#text", null);
      return currentNode;
    }
    function currentElement() {
      {
        assertInPatch("currentElement");
        assertNotInAttributes("currentElement");
      }
      return currentParent;
    }
    function currentPointer() {
      {
        assertInPatch("currentPointer");
        assertNotInAttributes("currentPointer");
      }
      return getNextNode();
    }
    function skip() {
      {
        assertNoChildrenDeclaredYet("skip", currentNode);
        setInSkip(true);
      }
      currentNode = currentParent.lastChild;
    }
    function createPatcher(run, patchConfig = {}) {
      const { matches: matches2 = defaultMatchFn } = patchConfig;
      const f = (node, fn, data) => {
        const prevContext = context;
        const prevDoc = doc;
        const prevFocusPath = focusPath;
        const prevArgsBuilder = argsBuilder;
        const prevAttrsBuilder = attrsBuilder;
        const prevCurrentNode = currentNode;
        const prevCurrentParent = currentParent;
        const prevMatchFn = matchFn;
        let previousInAttributes = false;
        let previousInSkip = false;
        doc = node.ownerDocument;
        context = new Context();
        matchFn = matches2;
        argsBuilder = [];
        attrsBuilder = [];
        currentNode = null;
        currentParent = node.parentNode;
        focusPath = getFocusedPath(node, currentParent);
        {
          previousInAttributes = setInAttributes(false);
          previousInSkip = setInSkip(false);
          updatePatchContext(context);
        }
        try {
          const retVal = run(node, fn, data);
          {
            assertVirtualAttributesClosed();
          }
          return retVal;
        } finally {
          context.notifyChanges();
          doc = prevDoc;
          context = prevContext;
          matchFn = prevMatchFn;
          argsBuilder = prevArgsBuilder;
          attrsBuilder = prevAttrsBuilder;
          currentNode = prevCurrentNode;
          currentParent = prevCurrentParent;
          focusPath = prevFocusPath;
          {
            setInAttributes(previousInAttributes);
            setInSkip(previousInSkip);
            updatePatchContext(context);
          }
        }
      };
      return f;
    }
    function createPatchInner(patchConfig) {
      return createPatcher((node, fn, data) => {
        currentNode = node;
        enterNode();
        fn(data);
        exitNode();
        {
          assertNoUnclosedTags(currentNode, node);
        }
        return node;
      }, patchConfig);
    }
    function createPatchOuter(patchConfig) {
      return createPatcher((node, fn, data) => {
        const startNode = { nextSibling: node };
        let expectedNextNode = null;
        let expectedPrevNode = null;
        {
          expectedNextNode = node.nextSibling;
          expectedPrevNode = node.previousSibling;
        }
        currentNode = startNode;
        fn(data);
        {
          assertPatchOuterHasParentNode(currentParent);
          assertPatchElementNoExtras(startNode, currentNode, expectedNextNode, expectedPrevNode);
        }
        if (currentParent) {
          clearUnvisitedDOM(currentParent, getNextNode(), node.nextSibling);
        }
        return startNode === currentNode ? null : currentNode;
      }, patchConfig);
    }
    var patchInner = createPatchInner();
    var patchOuter = createPatchOuter();
    var buffer = [];
    var bufferStart = 0;
    function queueChange(fn, a, b, c) {
      buffer.push(fn);
      buffer.push(a);
      buffer.push(b);
      buffer.push(c);
    }
    function flush() {
      const start = bufferStart;
      const end = buffer.length;
      bufferStart = end;
      for (let i = start; i < end; i += 4) {
        const fn = buffer[i];
        fn(buffer[i + 1], buffer[i + 2], buffer[i + 3]);
      }
      bufferStart = start;
      truncateArray(buffer, start);
    }
    var prevValuesMap = createMap();
    function calculateDiff(prev, next, updateCtx, updateFn) {
      const isNew = !prev.length;
      let i = 0;
      for (; i < next.length; i += 2) {
        const name = next[i];
        if (isNew) {
          prev[i] = name;
        } else if (prev[i] !== name) {
          break;
        }
        const value = next[i + 1];
        if (isNew || prev[i + 1] !== value) {
          prev[i + 1] = value;
          queueChange(updateFn, updateCtx, name, value);
        }
      }
      if (i < next.length || i < prev.length) {
        const startIndex = i;
        for (i = startIndex; i < prev.length; i += 2) {
          prevValuesMap[prev[i]] = prev[i + 1];
        }
        for (i = startIndex; i < next.length; i += 2) {
          const name = next[i];
          const value = next[i + 1];
          if (prevValuesMap[name] !== value) {
            queueChange(updateFn, updateCtx, name, value);
          }
          prev[i] = name;
          prev[i + 1] = value;
          delete prevValuesMap[name];
        }
        truncateArray(prev, next.length);
        for (const name in prevValuesMap) {
          queueChange(updateFn, updateCtx, name, void 0);
          delete prevValuesMap[name];
        }
      }
      flush();
    }
    var ATTRIBUTES_OFFSET = 3;
    var prevAttrsMap = createMap();
    function diffAttrs(element, data) {
      const attrsBuilder2 = getAttrsBuilder();
      const prevAttrsArr = data.getAttrsArr(attrsBuilder2.length);
      calculateDiff(prevAttrsArr, attrsBuilder2, element, updateAttribute);
      truncateArray(attrsBuilder2, 0);
    }
    function diffStatics(node, data, statics) {
      if (data.staticsApplied) {
        return;
      }
      data.staticsApplied = true;
      if (!statics || !statics.length) {
        return;
      }
      if (data.hasEmptyAttrsArr()) {
        for (let i = 0; i < statics.length; i += 2) {
          updateAttribute(node, statics[i], statics[i + 1]);
        }
        return;
      }
      for (let i = 0; i < statics.length; i += 2) {
        prevAttrsMap[statics[i]] = i + 1;
      }
      const attrsArr = data.getAttrsArr(0);
      let j = 0;
      for (let i = 0; i < attrsArr.length; i += 2) {
        const name = attrsArr[i];
        const value = attrsArr[i + 1];
        const staticsIndex = prevAttrsMap[name];
        if (staticsIndex) {
          if (statics[staticsIndex] === value) {
            delete prevAttrsMap[name];
          }
          continue;
        }
        attrsArr[j] = name;
        attrsArr[j + 1] = value;
        j += 2;
      }
      truncateArray(attrsArr, j);
      for (const name in prevAttrsMap) {
        updateAttribute(node, name, statics[prevAttrsMap[name]]);
        delete prevAttrsMap[name];
      }
    }
    function elementOpenStart(nameOrCtor, key2, statics) {
      const argsBuilder2 = getArgsBuilder();
      {
        assertNotInAttributes("elementOpenStart");
        setInAttributes(true);
      }
      argsBuilder2[0] = nameOrCtor;
      argsBuilder2[1] = key2;
      argsBuilder2[2] = statics;
    }
    function key(key2) {
      const argsBuilder2 = getArgsBuilder();
      {
        assertInAttributes("key");
        assert(argsBuilder2);
      }
      argsBuilder2[1] = key2;
    }
    function attr(name, value) {
      const attrsBuilder2 = getAttrsBuilder();
      {
        assertInPatch("attr");
      }
      attrsBuilder2.push(name);
      attrsBuilder2.push(value);
    }
    function elementOpenEnd() {
      const argsBuilder2 = getArgsBuilder();
      {
        assertInAttributes("elementOpenEnd");
        setInAttributes(false);
      }
      const node = open(argsBuilder2[0], argsBuilder2[1]);
      const data = getData(node);
      diffStatics(node, data, argsBuilder2[2]);
      diffAttrs(node, data);
      truncateArray(argsBuilder2, 0);
      return node;
    }
    function elementOpen(nameOrCtor, key2, statics, ...varArgs) {
      {
        assertNotInAttributes("elementOpen");
        assertNotInSkip("elementOpen");
      }
      elementOpenStart(nameOrCtor, key2, statics);
      for (let i = ATTRIBUTES_OFFSET; i < arguments.length; i += 2) {
        attr(arguments[i], arguments[i + 1]);
      }
      return elementOpenEnd();
    }
    function applyAttrs() {
      const node = currentElement();
      const data = getData(node);
      diffAttrs(node, data);
    }
    function applyStatics(statics) {
      const node = currentElement();
      const data = getData(node);
      diffStatics(node, data, statics);
    }
    function elementClose(nameOrCtor) {
      {
        assertNotInAttributes("elementClose");
      }
      const node = close();
      {
        assertCloseMatchesOpenTag(getData(node).nameOrCtor, nameOrCtor);
      }
      return node;
    }
    function elementVoid(nameOrCtor, key2, statics, ...varArgs) {
      elementOpen.apply(null, arguments);
      return elementClose(nameOrCtor);
    }
    function text$1(value, ...varArgs) {
      {
        assertNotInAttributes("text");
        assertNotInSkip("text");
      }
      const node = text();
      const data = getData(node);
      if (data.text !== value) {
        data.text = value;
        let formatted = value;
        for (let i = 1; i < arguments.length; i += 1) {
          const fn = arguments[i];
          formatted = fn(formatted);
        }
        if (node.data !== formatted) {
          node.data = formatted;
        }
      }
      return node;
    }
    exports.applyAttr = applyAttr;
    exports.applyProp = applyProp;
    exports.attributes = attributes;
    exports.alignWithDOM = alignWithDOM;
    exports.close = close;
    exports.createPatchInner = createPatchInner;
    exports.createPatchOuter = createPatchOuter;
    exports.currentElement = currentElement;
    exports.currentPointer = currentPointer;
    exports.open = open;
    exports.patch = patchInner;
    exports.patchInner = patchInner;
    exports.patchOuter = patchOuter;
    exports.skip = skip;
    exports.skipNode = nextNode;
    exports.setKeyAttributeName = setKeyAttributeName;
    exports.clearCache = clearCache;
    exports.getKey = getKey;
    exports.importNode = importNode;
    exports.isDataInitialized = isDataInitialized;
    exports.notifications = notifications;
    exports.symbols = symbols;
    exports.applyAttrs = applyAttrs;
    exports.applyStatics = applyStatics;
    exports.attr = attr;
    exports.elementClose = elementClose;
    exports.elementOpen = elementOpen;
    exports.elementOpenEnd = elementOpenEnd;
    exports.elementOpenStart = elementOpenStart;
    exports.elementVoid = elementVoid;
    exports.key = key;
    exports.text = text$1;
  }
});

// lib/idom.ts
var dom = __toESM(require_incremental_dom_cjs());
var idom_default = dom;
export {
  idom_default as default
};
/**
 * @preserve
 * Copyright 2015 The Incremental DOM Authors. All Rights Reserved.
 * Licensed under the Apache License, Version 2.0.
 */
//# sourceMappingURL=idom.js.map
