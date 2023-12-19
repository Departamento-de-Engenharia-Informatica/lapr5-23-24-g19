import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import { RoomName as Name } from './roomName'
import { RoomDescription as Description } from './description'
import { RoomDimensions } from './roomDimensions'
import { Floor } from '../floor/floor'
import { RoomCategory as Category } from './roomCategory'
import { Coordinates } from '../floor/Coordinates'

export interface RoomProps {
    name: Name
    category: Category
    description: Description
    floor: Floor
    dimensions: RoomDimensions
    positions: Coordinates
}

export default class Room extends AggregateRoot<RoomProps> {
    room: Result<Name>
    get id(): UniqueEntityID {
        return this._id
    }

    get floor(): Floor {
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

    get positions(): Coordinates {
        return this.props.positions
    }

    private constructor(props: RoomProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(dto: RoomProps, id?: UniqueEntityID): Result<Room> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
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
