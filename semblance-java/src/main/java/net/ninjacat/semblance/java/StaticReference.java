package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.utils.Try;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

/**
 * Static reference represented as {@code fully.qualified.class.Name.method} or {@code fully.qualified.class.Name.field}
 */
public class StaticReference {

    private final String member;
    private final Class<?> clazz;

    /**
     * Creates a new StaticReference
     *
     * @param reference Refernce name represented as atom
     */
    public StaticReference(final SymbolAtom reference) {
        this(reference.repr());
    }

    /**
     * Creates a new StaticReference
     *
     * @param reference Refernce name
     */
    public StaticReference(final String reference) {
        final int dot = reference.lastIndexOf('.');
        final String className;
        try {
            className = reference.substring(0, dot);
            member = reference.substring(dot + 1);
        } catch (final Exception ex) {
            throw new IllegalArgumentException("Invalid member reference format: " + reference, ex);
        }
        try {
            clazz = Class.forName(className);
        } catch (final Exception ex) {
            throw new JavaInteropException(String.format("Cannot get class for static method %s.", className), SourceInfo.UNKNOWN, ex);
        }
    }

    /**
     * @return Class of the reference
     */
    public Class<?> getClazz() {
        return clazz;
    }

    /**
     * @return Collection of all {@link Method}s with names matching to the one of this reference.
     */
    public Collection<Method> getMethods() {
        return Arrays.stream(clazz.getMethods())
                .filter(method -> member.equals(method.getName())).collect(Collectors.toList());
    }

    /**
     * @return Optional {@link Field} with the name matching the one of this reference.
     */
    public Optional<Field> getField() {
        return Try.execute(new Callable<Field>() {
            @Override
            public Field call() throws Exception {
                return clazz.getField(member);
            }
        }).get();
    }

    @Override
    public String toString() {
        return clazz.getCanonicalName() + "." + member;
    }
}
