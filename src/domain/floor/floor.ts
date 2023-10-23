import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'
import { FloorNumber } from './floorNumber'

import { IFloorDTO } from '../../dto/IFloorDTO'
import Building from '../building/building'
import { Description } from '../description'

export interface FloorProps {
    building: Building
    floorNumber: FloorNumber
    description?: Description
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
}
