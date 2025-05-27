# INF222 V25 - Obligatory Assignment 1

This assignment is split into two parts. In the first one you will implement a simple system in Kotlin to perform tax calculations for a model of Skatteetaten (the Norwegian Tax Administration). In the second part, you will use AspectJ to enable automated currency conversions in certain kind of Java programs.

## Deliverables

For the assignment you must deliver **this folder with your code as a ZIP file.**

## Prerequisites

To run the code examples provided in the tasks, ensure that you have the following installed on your system:

- Java Development Kit (JDK)
- Maven
- A Kotlin compiler

Make sure to have these tools properly set up and configured in your environment before proceeding.

## Running the tasks

### Part 1

Navigate to [part-1](./part-1) in the terminal and execute the command

```bash
kotlinc -script skatteetaten.kts
```

Alternatively, you can use an online IDE such as [Replit](https://replit.com) to run the script.

### Part 2

Navigate to [part-2](./part-2) in the terminal and execute the command

```bash
mvn exec:exec
```

> ⚠️ **Important:** **Make sure to use this command when running the code as otherwise load-time weaving will not be enabled, and AspectJ will not run.**

## Part 1 - Variance in Kotlin

In this task, you will build a tax calculation system for Skatteetaten (the Norwegian Tax Administration). Skatteetaten consists of several offices. Each office specializes in a certain category of taxpayers: _employee_, _business_, or _freelancer_. Each of the Skateetaten's offices applies different _tax rules_ based on the taxpayer category which that office specializes in.

Your task is to create a system that performs such tax calculations. To make this system both flexible and type-safe, you’ll need to think carefully about how to represent:

- taxpayers
- tax rules
- tax offices
  while ensuring the system can scale to handle various taxpayer categories.

In [skatteetaten.kts](./part-1/skatteetaten.kts), you are provided with some boilerplate code. The `main` function demonstrates how the system is expected to behave once fully implemented. **You should not change this method in any way.**

**Your job is to fill in the missing pieces**: namely, to implement the `TaxOffice` and `Skatteetaten` classes, such that everything works correctly.
Moreover:

- `TaxOffice` should support methods for registering a new taxpayer, assigning a new tax rule, and for calculating the taxes of all it's taxpayers.
- `Skatteetaten` should support methods for adding a new office, and retrieving all registered offices.

At first glance, the requirements may seem straightforward: register taxpayers, assign tax rules, and calculate taxes. However, as you build the system, you may encounter challenges that require a deeper understanding of the Kotlin's type system, particularly of variance and generic bounds.

There are no step-by-step instructions for how to implement everything.
Instead, **you are encouraged to explore, experiment, and determine the best way to achieve the desired functionality.**
The provided code should give you enough context to get started, but the underlying structure is up to you to design.
This is one way for you to get better at the general competence _"make justified decisions about the use of different programming language constructs in programming"_ ("_gjere grunngjeve slutningar om bruken av programmeringsspråkkonstruksjonar i programmering_") explicitly mentioned in the [INF222 course description](https://www4.uib.no/emner/inf222).

### Your Objective

- Implement a system that supports tax calculations for different categories of taxpayers.
- Design flexible tax rules that can be applied to specific taxpayer categories or all categories.
- Manage multiple tax rules within each tax office and calculate the total tax for each taxpayer.
- Handle type safety and discover how variance modifiers (`in`, `out`) can make your implementation more robust and flexible.

## Part 2 - Aspect-Oriented Programming in AspectJ

### 2.1 - Converting Currencies

In [Arithmetics](./part-2/src/main/java/inf222/aop/Arithmetics.java), you will find several arithmetic expressions involving static fields. Each of these fields has a currency specifier, such as `USD`, or `NOK`, appended directly to the name of the field (for example, `price_NOK`, `salary_USD`, etc.). The currency specifier indicates that the variable represents an amount in the specified currency.

When you run the code as it is, the compiler is obviously unaware of the fact variables' values are given in different currencies and have to be converted to actually be accurate values. For example, if we add `a_NOK + b_USD`, where `a_NOK` was declared as `double a_NOK=10;` and `b_USD` was declared as `b_USD=5;`, the result should not simply be `15`. Ideally, currencies should be converted to a common unit before performing arithmetic operations. In this case, assuming an exchange rate of `1 USD = 11 NOK`, the correct calculation would be:

> 10 NOK + 5 USD = 10 NOK + 55 NOK = 65 NOK

Your task is to implement such currency conversion for variables **without modifying the original source code**, but rather using **Aspect-Oriented Programming** techniques in **AspectJ**.

Here is a simple example of Java code and what should be its output when it's run via the AspectJ weaver:

```Java
double example_USD = 1.0;
System.out.println(example); // should output 11.0
```

### Requirements

In [CurrencyAspect](./part-2/src/main/java/inf222/aop/CurrencyAspect.java), **you need to write an advice that**:

- intercepts field accesses
- converts the value to NOK
- Note: You must use regular expressions to determine what currency the field corresponds to.

### Exchange Rates

You can use the following exchange rates when converting to NOK:

| Currency | Exchange rate to NOK |
| -------- | -------------------- |
| GBP      | 14                   |
| USD      | 11                   |
| DKK      | 1.5                  |
| JPY      | 0.07                 |

### 2.2 - Handling Field Modification

If you have implemented the advice in [2.1](#21),
field accesses should now be handled correctly.
However, this introduces a new problem: whenever we _modify_ a field, it will hold the wrong value.

Consider the following Java code where a variable is being modified:

```Java
double example_USD = 1;
example_USD *= 2; // the value it holds now is actually 22
System.out.println(example_USD) // will output 242
```

In this example, after modifying `example_USD`, the value that it holds is not what we would expect. This happens because compound assignment operators like `*=`, `+=`, etc. implicitly reference the field:

`example_USD *= 2;` is equivalent to `example_USD = example_USD * 2;`

In this case, the variable's reference is implicitly converted to NOK, and the resulting value is computed in terms of NOK rather than USD.
As a result, the field holds an incorrect value (which is actually in NOK).

Your task is to solve this issue by converting the value back to the correct currency.

### Requirements

In [CurrencyAspect](./part-2/src/main/java/inf222/aop/CurrencyAspect.java) **you need to write an advice that**:

- intercepts field modifications
- does not intercept the initalization of fields inside the constructor
- converts the value back to its original currency from NOK

<details>
  <summary><b>Hints</b></summary>
  <ul>
    <li>To reference execution of a constructor, you can create a control flow pointcut: <code>cflow(execution(..................))</code></li>
    <li>To change the value a field is being set to you need to use the <code>jp.proceed</code> method. It can be used as such:
      <pre><code class="language-java">@Around("......")
void someMethod(ProceedingJoinPoint jp) {
  jp.proceed(new Object[] {10.0});
}</code></pre>
    </li>

  </ul>
</details>

### 2.3 - Handling Negative Values

The program is now nearly complete, but there's one important edge case to address. Since we are dealing with currency values, they should never be negative (if we disregard loans and debt). To ensure that our program behaves correctly, we need to add a validation that ensures that any currency value remains positive.

### Requirements

For the last part, **you need to write an advice that**:

- intercepts field modifications
- checks whether the new value is positive
- if it's negative, throws an exception
