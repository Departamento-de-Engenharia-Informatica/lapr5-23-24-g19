import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'
import { FloorNumber } from './floorNumber'

import Building from '../building/building'
import { Description } from '../description'
import { FloorMapContent } from './floorMap'

export interface FloorProps {
    building: Building
    floorNumber: FloorNumber
    description?: Description
    map?: FloorMapContent
}

export class Floor extends AggregateRoot<FloorProps> {
    get id(): UniqueEntityID {
        return this._id
    }

    get building(): Building {
        return this.props.building
    }

    get floorNumber(): FloorNumber {
        return this.props.floorNumber
    }
    get map(): FloorMapContent {
        return this.props.map
    }

    set floorNumber(floorNumber: FloorNumber) {
        this.props.floorNumber = floorNumber
    }

    get description(): Description {
        return this.props.description
    }

    set description(description: Description) {
        this.props.description = description
    }

    private constructor(props: FloorProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(dto: FloorProps, id?: UniqueEntityID): Result<Floor> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: dto.building, argumentName: 'building' },
            { argument: dto.floorNumber, argumentName: 'floorNumber' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail<Floor>(guardResult.message)
        }else{
            return Result.ok<Floor>(new Floor({ ...dto }, id))

        }
    }

    public sameBuilding(floor2: Floor): boolean {
        return this.building.equals(floor2.building)
    }

    public addMap(newMap: FloorMapContent): boolean{
        if(this.building.fit(newMap.dimensions)){
            this.props.map = newMap
            return true
        }else{
            return false
        }
    }
    
}
