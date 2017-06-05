package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.errors.runtime.InternalSemblanceError;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;
import net.ninjacat.semblance.reader.Reader;

import javax.annotation.Nonnull;
import java.io.InputStream;

/**
 * Basic math library for Semblance
 */
public class MathLib implements ContextModifier {

    @Override
    public void modify(@Nonnull final Context context) {
        context.addNamespace(new MathNamespace());

        try (final InputStream is = getClass().getResourceAsStream("math.smbl")) {
            final Reader reader = new Reader();
            context.evaluateBlock(reader.read(is));
        } catch (Exception e) {
            throw new InternalSemblanceError("Cannot read or execute math.smbl");
        }
    }
}
