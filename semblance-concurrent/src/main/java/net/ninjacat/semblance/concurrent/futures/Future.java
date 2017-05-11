/*
 * Copyright 2014 Oleksiy Voronin <ovoronin@gmail.com>
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package net.ninjacat.semblance.concurrent.futures;


import net.ninjacat.semblance.utils.Try;

import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * <p>
 * Represents long-running operation that should be executed asynchronously.
 * </p>
 * <p>
 * Result of the operation is either a value or a {@link Throwable}. To get the result of operation
 * caller should supply handlers for success and/or failure.
 * </p>
 * <p>
 * Several operations can be chained by using {@link #then(Function)} method, for
 * example:
 * <pre>
 *     Future.run(callable).then(transform1).then(transform2).onSuccess(ok).onFailure(boom);
 * </pre>
 * This operation can fail at any time, but will never allow any exception escape. In the event that every
 * callable and transform completes successfully then {@link #onSuccess(Consumer)}
 * will be called, otherwise {@link #onFailure(Consumer)} )} will be called with
 * and exception as parameter. All the operations will be performed asynchronously.
 */
@SuppressWarnings("WeakerAccess")
public class Future<E> {
    private static final ExecutorService DEFAULT_EXECUTOR_SERVICE = Executors.newCachedThreadPool();
    private final ExecutorService executor;
    private final CountDownLatch latch;
    private volatile Consumer<E> successHandler;
    private volatile Consumer<Throwable> failHandler;
    private volatile Try<E> result;
    private volatile boolean executedOnce;

    /**
     * Creates a future with a default executor
     */
    public Future() {
        this(null);
    }

    /**
     * Creates a future with supplied executor service
     *
     * @param executor {@link ExecutorService} to be used for asynchronous execution
     */
    public Future(final ExecutorService executor) {
        this.executor = null == executor ? DEFAULT_EXECUTOR_SERVICE : executor;
        result = null;
        latch = new CountDownLatch(1);
    }

    /**
     * <p>
     * Shortcut for executing a {@link Callable} with default parameters.
     * </p>
     * <p>
     * Will create a new Future and execute it.
     * </p>
     *
     * @param block callable to be executed
     * @param <E>   type of the result
     * @return Future&lt;E&gt;
     */
    public static <E> Future<E> run(final Callable<E> block) {
        return new Future<E>().doIt(block);
    }

    /**
     * <p>
     * Allows chaining asynchronous operations.
     * </p>
     * <p>
     * Will start new supplied transform function within a new Future as soon as this future succeeds. In
     * case this future fails, failure cause will be propagated to the new Future.
     * </p>
     *
     * @param transform function to transform results of this future into result of a new future
     * @param <T>       type of the new future result
     * @return Future wrapping transform function
     */
    public final <T> Future<T> then(final Function<E, T> transform) {
        return new ChainableFuture<T, E>(this, transform, executor);
    }

    /**
     * <p>Sets successful execution handler.</p>
     * <p>This handler can be set only once. Will throw {@link IllegalStateException} if attempt to set
     * it again will be made</p>
     * <p>There are no guarantees when the callback will be called</p>
     *
     * @param onSuccess {@link Consumer} to handle successful Future execution
     * @return this future
     */
    public final Future<E> onSuccess(final Consumer<E> onSuccess) {
        Validators.validateNull(successHandler, new IllegalStateException("Cannot reassign onSuccess handler"));
        if (null != result && result.isSuccessful()) {
            onSuccess.accept(result.getValue());
        } else {
            successHandler = onSuccess;
        }
        return this;
    }

    /**
     * <p>Sets failed execution handler.</p>
     * <p>This handler can be set only once. Will throw {@link IllegalStateException} if attempt to set
     * it again will be made</p>
     * <p>There are no guarantees when the callback will be called</p>
     *
     * @param onFailure {@link Consumer} to handle failed Future execution
     * @return this future
     */
    public final Future<E> onFailure(final Consumer<Throwable> onFailure) {
        Validators.validateNull(failHandler, new IllegalStateException("Cannot reassign onFailure handler"));
        if (null != result && !result.isSuccessful()) {
            onFailure.accept(result.getFailure());
        } else {
            failHandler = onFailure;
        }
        return this;
    }

    /**
     * <p>
     * Executes block of code in the future.
     * </p>
     * <p>
     * Each future can execute code only once. Second attempt ot call this method will throw
     * {@link IllegalStateException}
     * </p>
     *
     * @param callable {@link Callable} that will be executed asynchronously
     * @return this future
     */
    public final Future<E> doIt(final Callable<E> callable) {
        if (executedOnce) {
            throw new IllegalStateException("Cannot execute more than once");
        }
        executedOnce = true;
        executor.submit(() -> {
            result = Try.execute(callable);
            if (result.isSuccessful()) {
                reportSuccess(result.getValue());
            } else {
                reportFailure(result.getFailure());
            }
            latch.countDown();
        });
        return this;
    }

    /**
     * Returns the result of the Future. This call will block if result is not available yet. Result is returned
     * wrapped in {@link Try} so that exceptions during future execution are captured.
     *
     * @return Result of the Future wrapped in Try
     */
    public Try<E> getResult() {
        try {
            latch.await();
            return result;
        } catch (final InterruptedException e) {
            return Try.failure(e);
        }
    }

    /**
     * Checks if execution is complete
     *
     * @return {@code true} if future is completed, {@code false} otherwise
     */
    public boolean isCompleted() {
        return latch.getCount() == 0;
    }

    private void reportSuccess(final E value) {
        if (null != successHandler) {
            successHandler.accept(value);
        }
    }

    private void reportFailure(final Throwable fail) {
        if (null != failHandler) {
            failHandler.accept(fail);
        }
    }

}
