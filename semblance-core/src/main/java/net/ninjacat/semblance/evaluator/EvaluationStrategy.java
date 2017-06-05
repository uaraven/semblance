package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;

/**
 * Evaluation strategy.
 * <p>
 * There is at least two implementations:
 * <ul>
 * <li>Runner</li>
 * <li>Debugger</li>
 * </ul>
 */
public interface EvaluationStrategy {

    /**
     * Evaluates single expression
     *
     * @param expression Expression to evaluate
     * @return Value of evaluated expression
     */
    LispValue evaluate(final LispValue expression);

    LispValue evaluateBlock(final LispCollection expressions);
}
