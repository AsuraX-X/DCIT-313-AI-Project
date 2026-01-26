# sim_SI.lisp — Simulation Runner (Overview) 🔧

This document explains the core code blocks in `sim_SI.lisp`, how the script detects being run, and how to run it from the shell or interactively.

## Purpose

`sim_SI.lisp` is a lightweight runner that:

- Loads the core agent implementation in `SI.lisp`.
- Exposes a simple `run-sim` function for interactive use.
- Detects when it's invoked as a script and runs automatically for convenience.

## Core blocks

### 1) Load implementation

```lisp
(load "SI.lisp")
```

Loads the agent code (`defstruct`, decision functions, environment helpers). Keep `SI.lisp` in the same directory or adjust the load path.

### 2) `run-sim` function

Signature:

```lisp
(defun run-sim (&key (tmax 10) (initial-moisture 25) (rain-times '(4)))
  ...)
```

Behavior:

- Creates an `agent` initialized with `initial-moisture`.
- Loops `time` from 1 to `tmax`.
- For each step:
  - Builds a `percept` (soil moisture, raining when `time` is a member of `rain-times`, and `time`).
  - Calls `update-model`, `decide-action`, and `execute-action` on the agent.
  - Updates moisture using `environment-step` and prints a line with time, moisture, action, and valve state.
- Returns the final `agent` object when finished.
