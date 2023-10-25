import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import { RoomId } from './roomId'

import { RoomName as Name } from './roomName'

import { RoomDescription as Description } from './description'
import { RoomDimensions } from './roomDimensions'
import Building from '../building/building'
import { Floor } from '../floor/floor'
import { RoomPositions } from './roomPositions'
import { RoomCategory as Category } from './roomCategory'

export interface RoomProps {
    building: Building
    floor: Floor
    name: Name
    description: Description
    category: Category
    dimensions: RoomDimensions
    positions: RoomPositions
}

export default class Room extends AggregateRoot<RoomProps> {
    room: Result<Name>
    get id(): UniqueEntityID {
        return this._id
    }

    get roomId(): RoomId {
        return RoomId.caller(this.id)
    }

    get building(): Building {
        return this.props.building
    }

    get floors(): Floor {
        return this.props.floor
    }

    get name(): Name {
        return this.props.name
    }

    get description(): Description {
        return this.props.description
    }

    get category(): Category {
        return this.props.category
    }

    get dimensions(): RoomDimensions {
        return this.props.dimensions
    }

    get positions(): RoomPositions {
        return this.props.positions
    }


    private constructor(props: RoomProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(dto: RoomProps, id?: UniqueEntityID): Result<Room> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: dto.building, argumentName: 'building' },
            { argument: dto.floor, argumentName: 'floor' },
            { argument: dto.name, argumentName: 'name' },
            { argument: dto.description, argumentName: 'description' },
            { argument: dto.category, argumentName: 'category' },
            { argument: dto.dimensions, argumentName: 'dimensions' },
            { argument: dto.positions, argumentName: 'positions' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else {
            return Result.ok(new Room({ ...dto }, id))
        }
    }
}
