export class Result<T> {
    public isSuccess: boolean
    public isFailure: boolean
    public error: T | string
    private _value: T

    public constructor(isSuccess: boolean, error?: T | string, value?: T) {
        if (isSuccess && error) {
            throw new Error(
                'InvalidOperation: A result cannot be successful and contain an error',
            )
        }
        if (!isSuccess && !error) {
            throw new Error(
                'InvalidOperation: A failing result needs to contain an error message',
            )
        }

        this.isSuccess = isSuccess
        this.isFailure = !isSuccess
        this.error = error
        this._value = value

        Object.freeze(this)
    }

    public getValue(): T {
        if (!this.isSuccess) {
            console.log(this.error)
            throw new Error(
                "Can't get the value of an error result. Use 'errorValue' instead.",
            )
        }

        return this._value
    }

    public getOrThrow(): T {
        if (this.isFailure) {
            throw new Error(JSON.stringify(this.errorValue()))
        }
        return this.getValue()
    }

    public errorValue(): T {
        return this.error as T
    }

    public static ok<U>(value?: U): Result<U> {
        return new Result<U>(true, null, value)
    }

    public static fail<U>(error: any): Result<U> {
        return new Result<U>(false, error)
    }

    public static combine(results: Result<any>[]): Result<any> {
        for (const result of results) {
            if (result.isFailure) return result
        }
        return Result.ok()
    }
}

export type Either<L, A> = Left<L> | Right<A>

export class Left<L> {
    readonly value: L

    constructor(value: L) {
        this.value = value
    }

    isLeft(): boolean {
        return true
    }

    isRight(): boolean {
        return false
    }
}

export class Right<A> {
    readonly value: A

    constructor(value: A) {
        this.value = value
    }

    isLeft(): boolean {
        return false
    }

    isRight(): boolean {
        return true
    }
}

export const left = <L, A>(l: L): Either<L, A> => {
    return new Left<L>(l)
}

export const right = <L, A>(a: A): Either<L, A> => {
    return new Right<A>(a)
}
