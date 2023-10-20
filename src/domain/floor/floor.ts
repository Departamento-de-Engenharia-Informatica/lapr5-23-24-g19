import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { FloorNumber } from './floorNumber'

import { IFloorDTO } from '../../dto/IFloorDTO'
import { Building } from '../building/building'
import { Description } from '../description'
import { FloorId } from './floorId'

interface FloorProps {
    building: Building
    floorNumber: FloorNumber
    description?: Description
}

export class Floor extends AggregateRoot<FloorProps> {
    get id(): UniqueEntityID {
        return this._id
    }

    get FloorId(): FloorId {
        return FloorId.caller(this.id)
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

    public static create(floorDTO: IFloorDTO, building: Building, id?: UniqueEntityID): Result<Floor> {
        try {
            const floor = new Floor(
                {
                    building,
                    floorNumber: FloorNumber.create(floorDTO.floorNumber).getValue(),
                    description: floorDTO.description ? Description.create(floorDTO.description).getValue() : undefined,
                },
                id,
            )

            return Result.ok<Floor>(floor)
        } catch (e) {
            return Result.fail<Floor>(e.message)
        }
    }
}
