package net.ninjacat.semblance.utils;

import com.google.common.collect.ImmutableList;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.collections.Vector;
import net.ninjacat.semblance.errors.runtime.InternalSemblanceError;

import java.util.EnumSet;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import static net.ninjacat.semblance.utils.Values.isList;
import static net.ninjacat.semblance.utils.Values.isVector;

/**
 * Java Stream collectors for Semblance collections
 */
public final class Collectors {

    private static final LispCollectionCollector<SList> SLIST_COLLECTOR =
            new LispCollectionCollector<>(lispValueBuilder -> new SList(lispValueBuilder.build()));

    private static final LispCollectionCollector<Vector> VECTOR_COLLECTOR =
            new LispCollectionCollector<>(lispValueBuilder -> new Vector(lispValueBuilder.build()));

    private Collectors() {
    }

    public static Collector<LispValue, ?, SList> toSList() {
        return SLIST_COLLECTOR;
    }

    public static Collector<LispValue, ?, Vector> toVector() {
        return VECTOR_COLLECTOR;
    }

    @SuppressWarnings("unchecked")
    public static <T extends LispCollection> Collector<LispValue, ?, T> toSameAs(final T template) {
        if (isList(template)) {
            return (Collector<LispValue, ?, T>) SLIST_COLLECTOR;
        } else if (isVector(template)) {
            return (Collector<LispValue, ?, T>) VECTOR_COLLECTOR;
        } else {
            throw new InternalSemblanceError("Cannot collect to " + template.getClass().getCanonicalName());
        }
    }


    private static class LispCollectionCollector<T extends LispCollection> implements Collector<LispValue, ImmutableList.Builder<LispValue>, T> {

        private final Function<ImmutableList.Builder<LispValue>, T> creator;

        private LispCollectionCollector(final Function<ImmutableList.Builder<LispValue>, T> creator) {
            this.creator = creator;
        }

        @Override
        public Supplier<ImmutableList.Builder<LispValue>> supplier() {
            return ImmutableList.Builder::new;
        }

        @Override
        public BiConsumer<ImmutableList.Builder<LispValue>, LispValue> accumulator() {
            return ImmutableList.Builder::add;
        }

        @Override
        public BinaryOperator<ImmutableList.Builder<LispValue>> combiner() {
            return (lispValueBuilder, lispValueBuilder2) -> new ImmutableList.Builder<LispValue>()
                    .addAll(lispValueBuilder.build())
                    .addAll(lispValueBuilder2.build());
        }

        @Override
        public Function<ImmutableList.Builder<LispValue>, T> finisher() {
            return creator::apply;
        }

        @Override
        public Set<Characteristics> characteristics() {
            return EnumSet.of(Characteristics.CONCURRENT);
        }
    }

}
