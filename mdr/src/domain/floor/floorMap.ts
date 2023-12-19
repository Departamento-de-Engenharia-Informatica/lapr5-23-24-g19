import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import { ValueObject } from '../../core/domain/ValueObject'
import { MaxFloorDimensions } from '../building/maxFloorDimensions'
import { Coordinates } from './Coordinates'

export interface FloorMapProps {
    path: string
    map?: {
        dimensions?: MaxFloorDimensions //Dimensions
        // mapContent: number[][],

        // passages: Coordinates[], //Coordinates
        // rooms: Coordinates[], //Coordinates
        // elevators: Coordinates[], //Coordinates
    }
    //textures
}

export class FloorMapContent extends ValueObject<FloorMapProps> {
    get path(): string {
        return this.path
    }
    // get mapContent(): number[][] {
    //     return this.props.map.mapContent
    // }

    // get passages(): Coordinates[] {
    //     return this.props.map.passages
    // }

    // get elevators(): Coordinates[] {
    //     return this.props.map.elevators
    // }

    // get rooms(): Coordinates[] {
    //     return this.props.map.rooms
    // }

    get dimensions(): MaxFloorDimensions {
        return this.props.map.dimensions
    }

    set dimensions(dimensions: MaxFloorDimensions) {
        this.dimensions = dimensions
    }

    private constructor(props: FloorMapProps) {
        super(props)
    }

    public static create(props: FloorMapProps): Result<FloorMapContent> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.map.dimensions, argumentName: 'dimensions' },
            // { argument: props.map.mapContent, argumentName: 'mapContent' },

            // { argument: props.map.elevators, argumentName: 'buildingDescription' },
            // { argument: props.map.passages, argumentName: 'buildingDescription' },
            // { argument: props.map.rooms, argumentName: 'buildingDescription' },
        ])
        if (guardResult.succeeded) {
            return Result.ok<FloorMapContent>(new FloorMapContent({ ...props }))
        } else {
            return Result.fail<FloorMapContent>(guardResult.message)
        }
    }
}
