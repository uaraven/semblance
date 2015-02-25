package net.ninjacat.semblance.data;

/**
 * Created on 24/02/15.
 */
public interface Function {
    LispValue apply(LispCollection parameters);
}
