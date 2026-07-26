# Conditions, restarts, and live computation

This is a research and design note about condition systems in Common
Lisp, Symbolics Genera, and Dylan, and about what Wisp might learn from
them.

The aim is not to reproduce any of these systems exactly. Wisp already
has unusually suitable raw material of its own: first-class delimited
continuations, deep effect handlers, serializable execution state,
browser interaction, and a Lisp heap that can contain the debugger
itself.

The interesting question is what kind of condition protocol naturally
belongs in that world.

## The central idea

Common Lisp does not primarily have "exceptions with retries." It has a
dynamically composed protocol for asking:

> Something happened; who knows what we should do about it?

Its pieces have deliberately limited responsibilities:

| Piece | What it knows |
| --- | --- |
| condition | What happened |
| signaler | Where it happened |
| handler | Policy: whether and how this situation matters |
| restart | Which recovery operations are actually possible here |
| debugger | How to let a human inspect the situation and choose |

An ordinary exception system tends to conflate several of these. A
throw both describes a problem and chooses to abandon a region of
computation. A catch both recognizes the problem and identifies where
control must go.

The Lisp condition tradition separates description, advice, recovery,
and control transfer. This is why it can support programmatic recovery
and an interactive debugger through the same protocol.

Kent Pitman emphasizes that condition handling is primarily a
*protocol* for connecting independently written pieces of software,
rather than merely a computational trick. If both ends are lexically
coordinated and written as one unit, an ordinary call or return is
usually clearer. Conditions become valuable when the code that detects
a situation cannot know the policy that should resolve it.

See [Condition Handling in the Lisp Language
Family](https://www.nhplace.com/kent/Papers/Condition-Handling-2001.html).

## Common Lisp

### Signaling does not imply unwinding

`signal` announces a condition to dynamically active handlers. It does
not itself unwind the stack, and an unhandled call to `signal` simply
returns `nil`.

At the primitive `handler-bind` level, each applicable handler is
called inside the dynamic context of the signaler. Returning from a
handler means:

> I decline to handle this condition; try the next handler.

The handler's returned values are ignored. Recovery occurs only if the
handler actively transfers control, normally by invoking a restart.

This is a remarkably useful constraint. Observing a condition cannot
accidentally become recovery. Logging, tracing, monitoring, policy,
debugging, and actual intervention can coexist along the same dynamic
chain.

The behavior is specified by the HyperSpec entries for
[`handler-bind`](https://www.lispworks.com/documentation/HyperSpec/Body/m_handle.htm)
and [`signal`](https://www.lispworks.com/documentation/HyperSpec/Body/f_signal.htm).

I checked this in the local SBCL 2.2.9:

```lisp
(handler-bind
    ((simple-condition
       (lambda (condition)
         (format t "inner: ~S~%" condition)
         17)))
  (handler-bind
      ((simple-condition
         (lambda (condition)
           (format t "outer: ~S~%" condition)
           23)))
    (signal "hello")))
```

Both handlers ran. Returning `17` and `23` did not handle the
condition. The search continued and `signal` returned `nil`.

### Pre-unwind and post-unwind handling

Common Lisp exposes two importantly different conveniences.

`handler-bind` invokes a handler before unwinding:

```lisp
(handler-bind
    ((error
       (lambda (condition)
         (declare (ignore condition))
         (invoke-restart 'use-value 41))))
  (+ 1 (failing-operation)))
```

If `failing-operation` offers a `use-value` restart, the handler can
still see and invoke it. In the local experiment, the operation used
`41` and the whole expression returned `42`.

`handler-case`, on the other hand, transfers control to its clause
after unwinding:

```lisp
(handler-case
    (failing-operation)
  (error (condition)
    ...))
```

A restart established inside `failing-operation` is normally gone by
the time this clause executes. The local experiment confirmed that
`handler-case` could not see the inner `use-value` restart while
`handler-bind` could.

This is not merely an implementation detail. Code that chooses among
the recovery operations offered by a failing computation usually needs
to run before those operations have been unwound away.

See the HyperSpec entry for
[`handler-case`](https://www.lispworks.com/documentation/HyperSpec/Body/m_hand_1.htm)
and Pitman's discussion of [handling in the context of the
signaler](https://www.nhplace.com/kent/Papers/Condition-Handling-2001.html).

### Restarts are recovery capabilities

A restart is a dynamically available recovery operation. It can have:

- a name used for programmatic lookup;
- a function that implements the recovery;
- a report describing the choice to a human;
- an interactive function that gathers arguments;
- a test restricting when the restart applies;
- an association with a particular condition.

They are reflectable catch points: code can ask which restarts are
available without invoking them. They are also capabilities in a
meaningful sense. Possessing a restart object grants the authority to
alter one particular suspended computation in one particular way.

The same restart may be selected by an automatic handler:

```lisp
(handler-bind
    ((malformed-field
       (lambda (condition)
         (declare (ignore condition))
         (invoke-restart 'use-value 0))))
  (parse-field "oops"))
```

or shown by the debugger to a human.

The [`restart-case`
reference](https://www.lispworks.com/documentation/HyperSpec/Body/m_rst_ca.htm)
specifies the report, interactive, and test machinery.
[`compute-restarts`](https://www.lispworks.com/documentation/HyperSpec/Body/f_comp_1.htm)
exposes the dynamic restart menu; the HyperSpec even sketches how a
portable debugger could present it.

At the most primitive level, a restart function does not have to
perform a nonlocal transfer. In another SBCL experiment, a restart
function returned `41` normally, `invoke-restart` returned that value
to its caller, and the interrupted body continued and added one. The
result was `42`.

Restarts are therefore less magical than their usual presentation.
They are dynamically discoverable functions that conventionally, but
not necessarily, transfer control.

### A complete small example

```lisp
(define-condition malformed-field (error)
  ((text
     :initarg :text
     :reader malformed-text)))

(defun parse-field (text)
  (restart-case
      (handler-case
          (parse-integer text)
        (error ()
          (error 'malformed-field :text text)))

    (use-value (value)
      :report "Use a replacement integer once."
      value)

    (retry-with (new-text)
      :report "Retry with different source text."
      (parse-field new-text))

    (skip-field ()
      :report "Skip this field."
      nil)))
```

`parse-field` does not decide whether malformed input should be
replaced, retried, skipped, or handed to a human. It *is* the code that
knows which recovery operations are meaningful.

A batch import can establish a handler that always chooses zero. A
strict caller can decline. An interactive listener can present the
three recovery choices. None of those policies need to be compiled
into `parse-field`.

This is the architecture in miniature:

- low-level code knows mechanism;
- high-level code knows policy;
- the condition carries facts between them;
- the debugger is the final interactive policy.

### Condition classification is separate from recovery

A condition describes what happened, not how it must be handled.

This matters because one event can belong to several useful
classifications. End-of-file while parsing may be both a stream
problem and a parsing problem. Different callers may be interested in
different aspects, but neither classification should hardwire a
recovery strategy.

Pitman argues in [Exceptional Situations in
Lisp](https://www.nhplace.com/kent/Papers/Exceptional-Situations-1990.html)
that a rich hierarchy, including multiple inheritance, is nearly
essential for this style of truthful classification.

Wisp does not need to begin with a CLOS-sized condition hierarchy. The
important lesson is the orthogonality. Structured values, tags, and
predicate matching can preserve it.

### Warnings and correctable errors are compositions

Common Lisp's `warn` establishes a `muffle-warning` restart and signals
a warning. If nobody selects the restart, the warning is printed. If a
handler invokes it, printing is suppressed. Execution continues either
way.

In the local SBCL experiment, a warning handler observed the warning,
invoked `muffle-warning`, and the surrounding computation returned
`:still-running` without the default warning output.

Likewise, `cerror` establishes a `continue` restart while signaling an
error. These are small standard protocols composed from the general
condition machinery, rather than unrelated runtime features.

See [`warn`](https://www.lispworks.com/documentation/HyperSpec/Body/f_warn.htm)
and [`cerror`](https://www.lispworks.com/documentation/HyperSpec/Body/f_cerror.htm).

### Condition-specific restarts

A dynamic environment may contain restarts belonging to several
simultaneously active situations. The debugger should not show an
unrelated `retry` simply because it happens to be somewhere on the
stack.

Common Lisp can associate restart objects with one condition through
[`with-condition-restarts`](https://www.lispworks.com/documentation/HyperSpec/Body/m_w_cnd_.htm).
Passing that condition to `find-restart` or `compute-restarts` filters
the result appropriately.

The local experiment established one `fix` restart and associated it
with one condition object. It was visible when restarts were computed
for that condition and absent for another instance of the same
condition class.

This small feature becomes crucial once a persistent browser debugger
contains several suspended evaluations and promise failures.

## Why this design is so good

The Common Lisp condition system was not invented in one act of
astronarchitecture.

Pitman's history traces it through Multics PL/I, the Symbolics Zetalisp
New Error System, Maclisp's shortcomings, and years of experience with
real interactive systems. The important features were harvested from
concrete failures:

- descriptions must not be confused with their printed messages;
- signaling must be separate from handling;
- condition classification must not imply recovery;
- interactive prompting must be separate from the recovery conduit;
- code must be able to inspect possible recovery without taking it;
- the computation should remain live while policy is consulted.

The Symbolics New Error System clearly separated:

1. establishing handlers;
2. managing proceed types, later called restarts;
3. detecting and signaling conditions.

Common Lisp changed some of its semantics. Symbolics used a passive
protocol in which a handler returned a proceed type and its arguments;
Common Lisp chose the active protocol in which a handler must invoke a
restart.

Pitman describes his work as transplanting and regularizing accumulated
experience, and admits that standardizing it still required a leap of
faith. That is part of what makes the result impressive: it is
simultaneously principled and marked by contact with actual programs.

## The Lisp Machine debugger

The language protocol becomes much easier to understand when seen in
the Symbolics Genera environment.

An unbound variable did not merely produce an error message and
backtrace. The debugger displayed recovery choices such as:

- supply a value for this use;
- supply a value and store it permanently;
- retry the instruction;
- abort to a particular enclosing listener activity.

These were selectable commands and mouse-sensitive objects. They were
the live computation's dynamically offered proceed and restart
handlers, presented to a human.

The debugger could also:

- evaluate Lisp forms in the suspended frame's lexical environment;
- inspect local variables and arguments;
- return a fabricated value from the current frame;
- edit and recompile the current function;
- reinvoke that frame using the new definition;
- optionally supply new arguments during reinvocation;
- move among other still-live processes and later return.

This is documented in the [Genera Program Development Utilities
manual](https://bitsavers.org/pdf/symbolics/software/genera_8/Program_Development_Utilities.pdf).
The contemporary [Genera Concepts
overview](https://www.chai.uni-hamburg.de/~moeller/symbolics-info/genera/genera.html)
describes repairing a live computation as an ordinary development
technique.

The debugger was not a stack-trace morgue. It was a workstation
attached to a live process.

An important distinction should nevertheless remain visible:

- `retry-this-read` is a semantic recovery authored by the operation;
- `return-from-this-arbitrary-frame` and
  `reinvoke-this-newly-compiled-function` are powers supplied by the
  development environment.

Both are excellent. They do not need to be represented as the same
kind of action.

## Dylan

Dylan keeps the condition-system family resemblance while making
several deliberately different decisions.

The [Dylan Programming Book chapter on
exceptions](https://package.opendylan.org/dylan-programming-book/exceptions.html)
provides an extended example. The [Dylan Reference Manual condition
chapter](https://opendylan.org/books/drm/Conditions) specifies the
protocol.

### Signaling is dynamic function lookup

The DRM explicitly describes two layers.

At the *signal layer*, the condition system makes a runtime connection
between a signaler and a handler. This is analogous to connecting an
ordinary caller and callee by function name. It is little more than a
way to locate and call a function and need not involve an exceptional
situation or nonlocal control flow.

The *exception layer* adds conventions for serious conditions,
recovery, exits, cleanup, and debuggers.

A handler established by `let handler` receives a condition and a
`next-handler` function:

```dylan
let handler <time-error>
  = method (condition, next-handler)
      ...
    end;
do-something();
```

Its semantics invert the Common Lisp convention:

| | Common Lisp | Dylan |
| --- | --- | --- |
| Handler returns | Declines | Handles |
| Explicit decline | Return normally | Tail-call `next-handler()` |
| Values returned by handler | Ignored | Returned from `signal` |
| Recovery by returning values | Not the handler protocol | Allowed when documented |

Calling `next-handler` tail-recursively preserves all values eventually
returned by a later handler. This matters because the current handler
may know only a superclass of the signaled condition and therefore may
not know its complete recovery protocol.

See [Signalers, Conditions, and
Handlers](https://opendylan.org/books/drm/Signalers_Conditions_and_Handlers).

### `signal` and `error`

In Dylan, `signal(condition)` may return any number of values from a
handler.

`error(condition)` promises never to return. If an ordinary handler
returns from a condition signaled with `error`, `error` invokes the
debugger. Recovery from an error must instead escape through a suitable
restart or another nonlocal exit.

The distinction makes intent explicit and may permit simpler compiled
code around `error`.

A condition class has a documented *recovery protocol* defining:

- whether returning from its handler is legal;
- what returned values mean;
- which restart classes the signaling operation promises to support.

For example, a hypothetical `<unbound-slot>` protocol might say that
returning one value uses it as the slot value and that a `<new-value>`
restart will be available with `value:` and `permanent:` fields.

The language does not dynamically enforce the types or number of
returned values. `signal` trusts the handler. Recovery protocols are
documented agreements, and a subclass is expected to remain compatible
with its superclass's protocol.

Dylan supplies introspective generic functions:

```dylan
return-allowed?(condition)
return-description(condition)
return-query(condition)
```

A debugger can therefore determine whether returning is permitted,
explain what it means, and query a human for suitable values.

See the DRM entries for [operations on
conditions](https://opendylan.org/books/drm/Operations_on_Conditions)
and [exception
handling](https://opendylan.org/books/drm/Exception_Handling).

### Calling handlers and exit handlers

Dylan exposes the pre-unwind and post-unwind models distinctly.

`let handler` executes the handler in the context of the signaler,
like an ordinary function call. The inner computation remains live.

An `exception` clause on `block` is catch-like:

```dylan
block ()
  use-resource();
cleanup
  release-resource();
exception (condition :: <resource-error>)
  report(condition);
end;
```

The exception handler takes a nonlocal exit to the beginning of the
block. Intervening cleanup clauses run before its body. Locals created
inside the block body are gone, and the block's exception handlers are
no longer active while the selected clause executes.

This corresponds roughly to the distinction between Common Lisp's
`handler-bind` and `handler-case`, but the control intent is unusually
clear in Dylan's surface syntax.

See the DRM entry for
[`block`](https://opendylan.org/books/drm/Statement_Macros).

### Restarts are conditions

This is Dylan's most striking change.

Common Lisp has parallel mechanisms:

```text
conditions ──▶ handlers
restarts   ──▶ restart functions
```

Dylan coalesces them:

```text
conditions ──▶ handlers
restarts are conditions ──▶ restart handlers
```

Every restart is an instance of a subclass of `<restart>`, which is
itself a subclass of `<condition>`. Requesting recovery means signaling
a restart condition.

The Programming Book's time example has this shape:

```text
low-level + installs a handler for <return-modulus-restart>
    │
    └─▶ + signals <time-boundary-error>
            │
            └─▶ outer policy handler receives the error
                    │
                    └─▶ signals <return-modulus-restart>
                            │
                            └─▶ low-level restart handler wraps the time
```

The original condition travels outward asking for policy. A restart
condition travels back inward requesting a particular repair.

A simplified version is:

```dylan
define class <return-modulus-restart> (<restart>)
end;

define method add-time (...)
  block ()
    if (invalid?)
      error(make(<time-boundary-error>, ...));
    else
      result;
    end;
  exception (restart :: <return-modulus-restart>)
    wrap-time(result);
  end;
end;
```

An outer handler can find and signal the offered restart:

```dylan
define method handle-time-error(condition, next-handler)
  let restart
    = available-restart(<return-modulus-restart>, condition);

  if (restart)
    error(restart);
  else
    next-handler();
  end;
end;
```

The restart handler performs a nonlocal exit to its block inside
`add-time`. The original call to `error` never returns, but `add-time`
can return normally with the recovered value.

This is best understood as a bidirectional condition conversation:

```text
problem description ──signal──▶ policy
recovery request    ──signal──▶ mechanism
```

### Discovering Dylan restarts

How can a debugger display restart conditions that have not yet been
constructed?

Dylan's dynamically installed handlers expose metadata:

- condition type;
- applicability test;
- handler function;
- initialization arguments.

`do-handlers` visits these descriptors from innermost to outermost. A
debugger or other recovery system can find handlers whose declared
types are restart classes, instantiate prospective restart conditions
from their initialization arguments, associate them with the original
condition, and apply their tests.

The resulting restart object can be shown to a human. `restart-query`
can interactively fill its slots before it is signaled.

Common Lisp stores a restart object pointing to a function. Dylan
stores a handler declaration from which a suitable restart request can
be constructed.

This is specified under [introspective operations on
conditions](https://opendylan.org/books/drm/Introspective_Operations_on_Conditions)
and [condition
operations](https://opendylan.org/books/drm/Operations_on_Conditions).

### The condition firewall

Common Lisp has a subtle behavior often called the *condition
firewall*. While a handler is executing, certain handlers established
more recently than it are no longer visible to newly signaled
conditions. This prevents an outward handler from accidentally
resignaling into handlers already passed during the current search.

Common Lisp restarts are a separate dynamic mechanism, so useful inner
restarts remain visible even though intervening condition handlers are
hidden.

Dylan removes this firewall. Its restarts are themselves conditions
handled by ordinary condition handlers, and the most useful restart
handlers usually live inside the operation that signaled the original
condition. Hiding those intervening handlers would also hide the
available recovery mechanisms.

`next-handler` controls continued processing of the *current* signal,
but a newly signaled condition can see dynamically intervening
handlers.

Pitman identifies this as a genuinely controversial design choice. He
wondered whether the resulting difficulty was evidence that restarts
should not have been made conditions, while carefully declining to
declare Dylan wrong without enough experience.

See the [Dylan discussion in Condition Handling in the Lisp Language
Family](https://www.nhplace.com/kent/Papers/Condition-Handling-2001.html).

### Dylan's condition hierarchy

Dylan's standard hierarchy also gives useful default semantics:

- `<condition>` may safely fall through; its default handler returns
  `#f`;
- `<warning>` prints a message and returns `#f`;
- `<serious-condition>` invokes the implementation-defined debugger if
  unhandled;
- `<error>` describes something invalid in the program;
- environmental failures can be serious without being program errors;
- `<restart>` defaults to an error when no restart handler accepts it;
- `<abort>` is a standard restart requesting escape to an application
  command loop or similar boundary.

The separation between `<error>` and `<serious-condition>` means that
code catching program errors need not accidentally catch resource
exhaustion or unpredictable environmental failures.

See the DRM [condition class
reference](https://opendylan.org/books/drm/Condition_Classes).

## Common Lisp and Dylan compared

The two designs agree on most of the deep architecture:

- conditions are structured objects;
- handlers are dynamically scoped;
- primitive handlers execute before unwind;
- condition description and recovery are separate;
- the debugger is a handler of last resort;
- programs and humans share the same recovery protocol;
- recovery capabilities can be inspected before selection;
- catch-like exit handling remains available as a convenience.

They disagree about how the conversation proceeds:

| Question | Common Lisp | Dylan |
| --- | --- | --- |
| How does a handler accept? | Invoke a restart or transfer control | Return values or transfer control |
| How does it decline? | Return normally | Call `next-handler()` |
| What is a restart? | Separate restart object and function | A condition handled by a restart handler |
| How are arguments supplied? | Arguments to the restart function | Slots in a restart condition |
| How are choices discovered? | `compute-restarts` | `do-handlers`, then instantiate restarts |
| Are intervening handlers hidden? | Yes, by the condition firewall | No |
| How is return recovery described? | Mostly conventional | `return-allowed?`, description, and query |

Common Lisp's active recovery makes observation safe by default:
returning cannot accidentally resolve anything.

Dylan's calling model is more uniform and expressive: signaling is
ordinary dynamic function dispatch with multiple return values, and
restarts use the same machinery. Its cost is a stronger reliance on
documented recovery protocols and more subtle recursive-signaling
semantics.

Neither is merely an exception mechanism. Both are systems for
negotiating the future of a live computation.

## Wisp's present position

Wisp already has most of the difficult control machinery.

In [`core/lisp/base.wisp`](core/lisp/base.wisp):

- `error` sends an `error` effect;
- `try` establishes an `error` prompt;
- `call-with-effect-handler` gives a deep handler the request plus
  explicit `resume` and `raise` functions;
- resumption reinstalls the effect handler;
- `raise` sends an error into the suspended continuation.

In [`web/js.wisp`](web/js.wisp), JavaScript promise completion and
rejection are expressed through that same deep-handler protocol.

In [`web/dexp.wisp`](web/dexp.wisp), the browser debugger captures:

- the condition;
- the suspended continuation;
- the original body for retry.

It currently offers four generic actions:

- use `nil`;
- supply a value;
- retry the entire body;
- abort.

This is an excellent first prototype. The important next seam is to
stop having the debugger invent semantic recovery and let the
suspended operation author its own menu.

## Possible Wisp directions

### Common Lisp geometry

One direction is an explicit restart object:

```lisp
(:restart use-value
 :condition condition
 :report "Use a replacement value once"
 :arguments ((value :wisp-expression))
 :invoke continuation)
```

An operation dynamically establishes these objects, and a handler or
debugger discovers and invokes one.

This is direct, reflective, and easy to render. It follows Common
Lisp's clean separation between the condition search and the restart
registry.

### Dylan geometry

A second direction treats a restart as another effect request:

```text
operation installs handler for use-value
    → operation signals malformed-field
        → policy handler signals use-value
            → operation's restart handler receives the request
```

This reuses one general handler mechanism and makes recovery a
two-directional protocol.

### Wisp geometry: send recovery into a suspended continuation

Wisp can potentially obtain Dylan's symmetry without relying on an
ambient no-firewall handler environment.

`call-with-effect-handler` already captures the suspended continuation
and supplies `raise`, which sends an error into it. A condition handler
could similarly receive a capability for sending a restart request
into that exact suspended computation:

```text
operation installs a local restart effect handler
    │
    └─▶ condition escapes to an outer policy handler
            │
            └─▶ policy sends a restart request into the captured continuation
                    │
                    └─▶ local restart handler performs the repair
```

The continuation becomes both:

- the suspended future of the operation;
- an address for communicating with the still-live inner dynamic
  environment.

This seems especially native to Wisp:

- it is delimited;
- the destination is explicit;
- the restart authority is capability-like;
- it naturally supports asynchronous suspension;
- the request and its metadata can be represented as Lisp data;
- it need not expose unrelated handlers in the ambient environment.

The handler interface might conceptually become:

```lisp
(fn (condition resume raise decline restart)
  ...)
```

where `restart` can only send structured recovery requests into the
captured continuation associated with this condition. The actual
surface syntax should follow from experiments rather than from this
placeholder.

### Restart descriptors as UI

Whichever control geometry Wisp chooses, the debugger needs structured
metadata rather than hardcoded buttons:

```lisp
(:restart retry-with
 :condition condition-id
 :report "Retry with different source text"
 :arguments
 ((source :type string
          :label "Source text")))
```

The description should support:

- a stable programmatic identity;
- a human presentation;
- declarative arguments;
- association with a particular condition;
- a callable or addressable recovery capability;
- an applicability predicate;
- perhaps provenance describing which frame or operation offered it.

Common Lisp's `:interactive` function is designed for a terminal. Wisp
can instead make interaction declarative and DEXP-native. An argument
can be a Lisp expression, string, choice, file, capability, heap object,
or workspace selection.

The same descriptor can drive:

- an interactive browser card;
- an automatic condition policy;
- a remote client;
- saved debugger state;
- a future dashboard acting on many related suspended conditions.

### Handler return versus explicit resumption

Wisp should make a deliberate choice about normal handler return.

Common Lisp says return means decline. Dylan says return handles and
becomes the value of `signal`. Wisp's existing effect handlers already
make resumption explicit through a supplied `resume` function.

That suggests a particularly robust Wisp convention:

- returning from the handler returns from the enclosing handled
  computation;
- `resume(value)` explicitly resumes the signaler;
- `decline()` explicitly consults an outer handler;
- `restart(request)` explicitly asks the suspended inner computation
  to perform a recovery.

This is neither Common Lisp nor Dylan exactly, but the control effects
would remain visible in source. It avoids assigning too many meanings
to ordinary function return.

Another plausible choice is to make normal return mean decline, as in
Common Lisp, because it makes observational handlers safe. This should
be settled through tiny programs before a public condition syntax is
designed.

### Recovery protocols should be data

Dylan's `return-allowed?`, `return-description`, and `return-query`
recognize that returning from a handler is itself a recovery operation
that needs to be described to tools.

Its weakness is that the complete recovery protocol remains informal
documentation.

Wisp can make more of that protocol inspectable:

```lisp
(:condition malformed-field
 :fields ((source string))
 :resume (:value integer
          :report "Use an integer as this field's value")
 :restarts (...))
```

This need not be a static type system. It is enough for tools and
humans to have truthful structured descriptions of the available
conversation.

### Conditions and capabilities

The restart set is an authority surface.

An inner operation may be willing to:

- retry one read;
- substitute one value;
- skip one record;
- use a different reader;
- abort to one enclosing evaluation.

It does not thereby grant authority to replace global standard input,
mutate an arbitrary binding, or abort the whole workspace.

Passing only selected restart capabilities outward is attenuation.
This connects the condition design directly to Wisp's explicit `cap`
and handled I/O direction.

For example, a failed read might offer:

- retry using the same reader;
- use an end-of-file value;
- retry using a supplied reader capability.

It should not be able to conjure a broader I/O capability by entering
the debugger. A higher layer that possesses such authority may choose
to supply it through an offered restart.

### Async conditions

Promise rejection already enters Wisp through an effect handler. A
suspended asynchronous condition could offer domain-specific
recoveries:

- retry the request;
- retry against another endpoint;
- use cached data;
- refresh an expired capability;
- substitute a value;
- abort only the enclosing evaluation.

A recovery action may itself await a promise. The debugger card can
remain as a durable representation of the suspended computation while
that happens.

Because Wisp serializes continuations and debugger state, a condition
could eventually remain recoverable across a page reload. That is much
closer to the Lisp Machine conception of live computation than the
usual ephemeral stack debugger.

### Operation restarts and debugger powers

The UI should distinguish at least two categories.

Program-authored recovery:

- use a value;
- skip a field;
- retry a request;
- grant or substitute a capability;
- abort to a named application boundary.

Development-environment intervention:

- inspect a continuation frame;
- evaluate in its lexical environment;
- return a value from a frame;
- edit and recompile a function;
- reinvoke a frame with old or new arguments.

The first category is part of the program's recovery protocol. The
second is the debugger operating on Wisp's first-class execution
representation. A Genera-like UI can present both without pretending
they have identical semantics.

## A promising first demonstration

A small initial example could exercise the complete architecture:

```text
parse integer field
    malformed-field condition
    offered recovery:
      use-value(integer)
      retry-with(string)
      skip-field()
      abort-to-evaluation()
```

Then run it in three contexts:

1. An outer batch handler automatically invokes `use-value(0)`.
2. A stricter handler declines, allowing the next policy to decide.
3. With no program policy, the browser debugger renders the same four
   recovery capabilities as DEXP controls.

The useful test is not merely whether all three work. It is whether the
low-level parser contains no batch, browser, or debugger policy and
whether the debugger contains no parser-specific recovery logic.

A second demonstration should cross an asynchronous boundary:

```text
fetch configuration
    network-failure condition
    offered recovery:
      retry()
      retry-with(url)
      use-cached()
      abort-to-evaluation()
```

If both examples use the same protocol, the design is probably finding
the right level.

## Suggested order of experiments

1. Represent condition and restart descriptions as ordinary Wisp data.
2. Let one operation establish honest, operation-specific recovery
   actions.
3. Change the last-resort debugger to enumerate those actions instead
   of inventing `use nil` and whole-body `retry`.
4. Add explicit decline and test nested handler behavior.
5. Prototype sending a restart request into the captured continuation.
6. Associate recovery choices with one condition instance.
7. Make one restart gather structured arguments through DEXP.
8. Repeat the experiment across `await`.
9. Express `warn` and `cerror`-like conveniences as small library
   protocols.
10. Only then decide whether Wisp needs named condition classes,
    inheritance, or a more formal recovery-protocol language.

The mechanism should earn its surface syntax through these examples.

## The larger possibility

The deepest shared lesson from Common Lisp, Genera, and Dylan is that a
failure does not have to mean that computation has become past tense.

A condition can be:

- a structured account of uncertainty;
- a request for policy;
- a rendezvous between separately written code;
- an invitation to inspect a live future;
- a point at which authority can be offered narrowly;
- a persistent object in an interactive workspace.

Common Lisp models possible futures as dynamically named restarts.
Dylan models recovery as another condition traveling back toward the
mechanism. Genera makes the whole protocol tangible as a live
development environment.

Wisp has the opportunity to make the suspended continuation itself an
inspectable, serializable, explicitly addressable participant in that
conversation.

That would not merely imitate an old Lisp facility. It would continue
the line of thought.
