package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.BaseNamespace;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.java.Lambdas.methodAsFunction;
import static net.ninjacat.semblance.utils.Values.*;

/**
 * Math namespace
 */
final class MathNamespace extends BaseNamespace {
    /**
     * Creates a new math namespace
     */
    MathNamespace() {
        super(symbol("math"));

        bind(symbol("sin"), methodAsFunction(MathNamespace::sin));
        bind(symbol("cos"), methodAsFunction(MathNamespace::cos));
        bind(symbol("tan"), methodAsFunction(MathNamespace::tan));
        bind(symbol("asin"), methodAsFunction(MathNamespace::asin));
        bind(symbol("acos"), methodAsFunction(MathNamespace::acos));
        bind(symbol("atan"), methodAsFunction(MathNamespace::atan));
        bind(symbol("atan2"), methodAsFunction(MathNamespace::atan2));
        bind(symbol("log"), methodAsFunction(MathNamespace::log));
        bind(symbol("exp"), methodAsFunction(MathNamespace::exp));
        bind(symbol("sqrt"), methodAsFunction(MathNamespace::sqrt));
        bind(symbol("to-radians"), methodAsFunction(MathNamespace::toRad));
        bind(symbol("to-degrees"), methodAsFunction(MathNamespace::toDeg));
    }

    private static LispValue sin(final Context context, final LispCollection params) {
        return doubleN(Math.sin(asNumber(params.head()).doubleValue()));
    }

    private static LispValue cos(final Context context, final LispCollection params) {
        return doubleN(Math.cos(asNumber(params.head()).doubleValue()));
    }

    private static LispValue asin(final Context context, final LispCollection params) {
        return doubleN(Math.asin(asNumber(params.head()).doubleValue()));
    }

    private static LispValue acos(final Context context, final LispCollection params) {
        return doubleN(Math.acos(asNumber(params.head()).doubleValue()));
    }

    private static LispValue tan(final Context context, final LispCollection params) {
        return doubleN(Math.tan(asNumber(params.head()).doubleValue()));
    }

    private static LispValue atan(final Context context, final LispCollection params) {
        return doubleN(Math.atan(asNumber(params.head()).doubleValue()));
    }

    private static LispValue atan2(final Context context, final LispCollection params) {
        return doubleN(Math.atan2(asNumber(params.head()).doubleValue(), asNumber(params.tail().head()).doubleValue()));
    }

    private static LispValue log(final Context context, final LispCollection params) {
        return doubleN(Math.log(asNumber(params.head()).doubleValue()));
    }

    private static LispValue exp(final Context context, final LispCollection params) {
        return doubleN(Math.exp(asNumber(params.head()).doubleValue()));
    }

    private static LispValue sqrt(final Context context, final LispCollection params) {
        return doubleN(Math.sqrt(asNumber(params.head()).doubleValue()));
    }

    private static LispValue toRad(final Context context, final LispCollection params) {
        return doubleN(Math.toRadians(asNumber(params.head()).doubleValue()));
    }

    private static LispValue toDeg(final Context context, final LispCollection params) {
        return doubleN(Math.toDegrees(asNumber(params.head()).doubleValue()));
    }

}
