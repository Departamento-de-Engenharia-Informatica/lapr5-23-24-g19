import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'
import { FloorNumber } from './floorNumber'

import { IFloorDTO } from '../../dto/IFloorDTO'
import { IFloorMapDTO } from '../../dto/IFloorMapDTO'
import Building from '../building/building'
import { Description } from '../description'
import { FloorMapContent } from './floorMap'
import { MaxFloorDimensions } from '../building/maxFloorDimensions'
import { Coordinates } from './Coordinates'

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

    get description(): Description {
        return this.props.description
    }

    private constructor(props: FloorProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(dto: FloorProps, id?: UniqueEntityID): Result<Floor> {
            const guardResult = Guard.againstNullOrUndefinedBulk([
                { argument: dto.floorNumber, argumentName: 'floorNumber' },
                { argument: dto.description, argumentName: 'buildingDescription' },
                { argument: dto.building, argumentName: 'building' },
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
    
    public addMap(dto: IFloorMapDTO): boolean{

        const mapOrError = FloorMapContent.create({
            dimensions: MaxFloorDimensions.create(dto.dimensions.length, dto.dimensions.width).getValue(),
            mapContent: dto.mapContent,
            passages: dto.passages.map(passage => Coordinates.create(passage.x, passage.y).getValue()),
            elevators: dto.elevators.map(elevator => Coordinates.create(elevator.x, elevator.y).getValue()),
            rooms: dto.rooms.map(room => Coordinates.create(room.x, room.y).getValue())
        });

        if(mapOrError.isSuccess){
            this.props.map=mapOrError.getValue()
            return true
        }else{
            return false
        }
    }
}
