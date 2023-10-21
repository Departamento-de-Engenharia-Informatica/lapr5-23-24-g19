import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface BuildingNameProps {
    value: string
}

const maxNameLength = 50

export class BuildingName extends ValueObject<BuildingNameProps> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: BuildingNameProps) {
        super(props)
    }

    public static create(name: string): Result<BuildingName> {
        if (!!name === false || name.length > maxNameLength) {
            return Result.fail<BuildingName>(`The name of the building must have at most ${maxNameLength} characters`)
        } else {
            return Result.ok<BuildingName>(new BuildingName({ value: name }))
        }
    }
}
