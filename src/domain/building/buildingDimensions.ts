import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface BuildingDimensionsProps {
    value: { length: number; width: number }
}

export class BuildingDimensions extends ValueObject<BuildingDimensionsProps> {
    get value(): { length: number; width: number } {
        return this.props.value
    }

    private constructor(props: BuildingDimensionsProps) {
        super(props)
    }

    public static create(dimensions: { length: number; width: number }): Result<BuildingDimensions> {
        const guardResult = Guard.againstNullOrUndefined(dimensions, 'dimensions')
        if (!guardResult.succeeded) {
            return Result.fail<BuildingDimensions>(guardResult.message)
        } else if (dimensions.length <= 0 || dimensions.width <= 0) {
            return Result.fail<BuildingDimensions>('Dimensions must have positive values for length and width')
        } else {
            return Result.ok<BuildingDimensions>(new BuildingDimensions({ value: dimensions }))
        }
    }
}
