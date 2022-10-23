# mermaid-clj

This fork aims to:

- Completely rewrite the original as well as make syntax changes,
- Update the original to the current Sequence Diagrams, and
- Extend the domain to other diagrams, such as Flowcharts, Class Diagrams, and more.

## Usage

The following DSL program translates to:

```clojure
(sequence-diagram
     (loop- "until dead"
          (solid-arrow :alice :bob "hihi")
          (solid-arrow :bob :alice "hoho")
          (optional "hoho"
                (solid-arrow :alice :bob "hihi")
                (alternative
                     ["x = 1" [(solid-arrow :alice :bob "hihi")]]
                     ["x = 2" [(solid-arrow :bob :john "hihi")]]
                     ["x = 3" [(solid-arrow :john :alice "hihi")]])))
     (parallel
       ["alice to bob" [(solid-arrow :alice :bob "hihi")]]
       ["bob to alice" [(solid-arrow :bob :alice "hihi")]]))
```

```mermaid
sequenceDiagram
    loop until dead
        alice->>bob: hihi
        bob->>alice: hoho
        opt hoho
            alice->>bob: hihi
            alt x = 1
                alice->>bob: hihi
            else x = 2
                bob->>john: hihi
            else x = 3
                john->>alice: hihi
            end
        end
    end
    par alice to bob
        alice->>bob: hihi
    and bob to alice
        bob->>alice: hihi
    end
```
