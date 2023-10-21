import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface BuildingDescriptionProps {
    value: string
}

const maxDescriptionLength = 255

export class BuildingDescription extends ValueObject<BuildingDescriptionProps> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: BuildingDescriptionProps) {
        super(props)
    }

    public static create(description: string): Result<BuildingDescription> {
        if (!!description === false || description.length > maxDescriptionLength) {
            return Result.fail<BuildingDescription>(
                `The Description must be at most ${maxDescriptionLength} characters`,
            )
        } else {
            return Result.ok<BuildingDescription>(new BuildingDescription({ value: description }))
        }
    }
}
