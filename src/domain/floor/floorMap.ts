import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import { ValueObject } from '../../core/domain/ValueObject'
import { MaxFloorDimensions } from '../building/maxFloorDimensions'
import { Coordinates } from './Coordinates'

export interface FloorMapProps {
    dimensions: MaxFloorDimensions //Dimensions
    mapContent: number[][]

    passages: Coordinates[] //Coordinates
    rooms: Coordinates[] //Coordinates
    elevators: Coordinates[] //Coordinates
}

export class FloorMapContent extends ValueObject<FloorMapProps> {

    get mapContent(): number[][] {
        return this.props.mapContent
    }

    get passages(): Coordinates[] {
        return this.props.passages
    }

    get elevators(): Coordinates[] {
        return this.props.elevators
    }

    get rooms(): Coordinates[] {
        return this.props.rooms
    }

    get dimensions(): MaxFloorDimensions {
        return this.props.dimensions
    }

    private constructor(props: FloorMapProps) {
        super(props)
    }

    public static create(props: FloorMapProps): Result<FloorMapContent> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.dimensions, argumentName: 'dimensions' },
            { argument: props.mapContent, argumentName: 'mapContent' },
            
            { argument: props.elevators, argumentName: 'buildingDescription' },
            { argument: props.passages, argumentName: 'buildingDescription' },
            { argument: props.rooms, argumentName: 'buildingDescription' },
        ])
        if (guardResult.succeeded) {
            return Result.ok<FloorMapContent>(new FloorMapContent({ ...props }))
        }else{
            return Result.fail<FloorMapContent>(guardResult.message)

        }
    }
}
