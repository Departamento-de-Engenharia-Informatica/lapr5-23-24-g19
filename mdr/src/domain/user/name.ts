import { ValueObject } from "../../core/domain/ValueObject"
import { Result } from "../../core/logic/Result"

interface Props {
    value: string
}

export class Name extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(code: string) {
        return Result.ok(new Name({ value: code }))
    }
}
