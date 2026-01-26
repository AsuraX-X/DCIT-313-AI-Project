# Smart Irrigation — Model-Based Reflex Agent

A compact Common Lisp implementation of a model-based reflex agent for a smart irrigation system.

## Project structure

- `SI.lisp` — core agent implementation (data structures, decision logic, actuator, environment step)
- `sim_SI.lisp` — simulation runner with `run-sim` (scriptable and interactive)
- `test_SI.lisp` — small test runner for unit tests
- `SI.md` / `sim_SI.md` — human-readable explanations of core code blocks
- `run_sim.sh` — convenience wrapper to run the simulation

## Quick start

Run the simulation (default):

```sh
sbcl --script sim_SI.lisp
# or with explicit flag:
sbcl --script sim_SI.lisp -- --run-sim
# or use the wrapper:
./run_sim.sh
```

Run unit tests:

```sh
sbcl --script test_SI.lisp
```

Interactive development (REPL):

```sh
sbcl --eval "(load \"SI.lisp\")" --eval "(load \"sim_SI.lisp\")"
;; then call (run-sim :tmax 20 :initial-moisture 30 :rain-times '(4 9)) in the REPL
```

## Notes

- Valve is represented by symbols `'OPEN` / `'CLOSED`.
- `decide-action` uses low/high thresholds and a `cooldown` (default 3 steps) to prevent frequent toggling.
- Tests are lightweight and dependency-free so they run with plain SBCL.

---