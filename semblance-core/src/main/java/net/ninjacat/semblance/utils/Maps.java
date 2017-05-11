package net.ninjacat.semblance.utils;

import java.util.HashMap;
import java.util.Map;

/**
 * Ported from Smooth
 */
public final class Maps {

    public static <K, V> Map<K, V> of(final Object... pairs) {
        if (0 != pairs.length % 2) {
            throw new IllegalArgumentException("Initializer should have even number of elements");
        }
        final Map<K, V> map = new HashMap<K, V>();

        for (int i = 0; i < pairs.length; i += 2) {
            map.put((K) pairs[i], (V) pairs[i + 1]);
        }

        return map;
    }
}
