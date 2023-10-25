import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    name: string
}

const maxLength = 50

export class RoomName extends ValueObject<Props> {
    get value(): string {
        return this.props.name
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(name?: string): Result<RoomName> {
        name = name ?? ''
        if (name.length > maxLength) {
            return Result.fail(`The name of the room must have at most ${maxLength} characters`)
        } else {
            return Result.ok(new RoomName({ name }))
        }
    }
}
