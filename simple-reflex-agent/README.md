# Smart Irrigation System – Simple Reflex Agent (LISP)

## Overview

This project implements a **Simple Reflex Agent** in Common Lisp for a Smart Irrigation System.  
The agent makes watering decisions based only on the current environmental conditions using **IF–THEN rules**.

The system does not store past data and does not use memory.  
All decisions are based purely on the current percept.

---

## Agent Type

**Simple Reflex Agent**

A Simple Reflex Agent:
- Uses only the current percept
- Has no memory of previous states
- Uses condition–action (IF–THEN) rules
- Does not learn or optimize decisions

---

## Environment Percepts

The irrigation agent receives three inputs:

- `soil-moisture` → `dry`, `moist`, `wet`
- `temperature` → `high`, `medium`, `low`
- `rain` → `yes`, `no`

---

## Decision Rules (IF–THEN Rules)

1. IF soil is dry AND temperature is high AND no rain → `water-heavy`
2. IF soil is dry AND no rain → `water-light`
3. IF soil is moist OR wet OR raining → `no-water`
4. Otherwise → `no-action`

---

## Lisp Implementation

```lisp
(defun irrigation-agent (soil-moisture temperature rain)
  (cond
    ((and (eq soil-moisture 'dry)
          (eq temperature 'high)
          (eq rain 'no))
     'water-heavy)

    ((and (eq soil-moisture 'dry)
          (eq rain 'no))
     'water-light)

    ((or (eq soil-moisture 'moist)
         (eq soil-moisture 'wet)
         (eq rain 'yes))
     'no-water)

    (t 'no-action)))
