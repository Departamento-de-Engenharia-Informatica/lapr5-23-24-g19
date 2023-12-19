import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

const maxLength = 50

interface Props {
    name: string
}

export class RoomName extends ValueObject<Props> {
    private constructor(name: Props) {
        super(name)
    }

    get value(): string {
        return this.props.name
    }

    public static create(name: string): Result<RoomName> {
        if (name.length <= maxLength) {
            return Result.ok(new RoomName({ name }))
        } else {
            return Result.fail(
                `The name of the room must have at most ${maxLength} characters`,
            )
        }
    }
}
