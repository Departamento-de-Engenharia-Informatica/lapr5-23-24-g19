import { ValueObject } from '../../core/domain/ValueObject'
import { Guard } from '../../core/logic/Guard'
import { Result } from '../../core/logic/Result'

interface Props {
    name: string
}

const maxLength = 50

export class BuildingName extends ValueObject<Props> {
    get value(): string {
        return this.props.name
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(name: string): Result<BuildingName> {
        const guardResult = Guard.againstNullOrUndefined(name, 'name')

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        }

        name = name.trim()
        if (name.length > maxLength) {
            return Result.fail(
                `The name of the building must have at most ${maxLength} characters`,
            )
        }

        return Result.ok(new BuildingName({ name }))
    }
}
