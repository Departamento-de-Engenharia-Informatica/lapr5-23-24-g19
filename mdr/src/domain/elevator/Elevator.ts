import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { Guard } from '../../core/logic/Guard'
import { Result } from '../../core/logic/Result'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import Building from '../building/building'
import { ElevatorBrand as Brand } from './brand'
import { ElevatorDescription as Description } from './description'
import { Floor } from '../floor/floor'
import { ElevatorIdentifier as Identifier } from './identifier'
import { ElevatorModel as Model } from './model'
import { ElevatorSerialNumber } from './serialNumber'

interface Props {
    building: Building
    identifier: Identifier
    floors: Floor[]

    brand?: Brand
    model?: Model
    serialNumber?: ElevatorSerialNumber
    description?: Description
}

export default class Elevator extends AggregateRoot<Props> {
    private constructor(props: Props, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(dto: Props, id?: UniqueEntityID): Result<Elevator> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: dto.building, argumentName: 'elevatorBuilding' },
            { argument: dto.floors, argumentName: 'elevatorFloors' },
            { argument: dto.identifier, argumentName: 'elevatorIdentifier' },
        ])

        if (!guardResult.succeeded || dto.floors.length < 1) {
            return Result.fail(guardResult.message ?? '')
        } else if (!!dto.brand && !dto.model) {
            return Result.fail('Model cannot be empty if Brand is specified')
        }

        return Result.ok(new Elevator({ ...dto }, id))
    }

    get id(): UniqueEntityID {
        return this._id
    }

    get brand(): Brand {
        return this.props.brand
    }

    get model(): Model {
        return this.props.model
    }

    get building(): Building {
        return this.props.building
    }

    get floors(): Floor[] {
        return this.props.floors
    }

    get identifier(): Identifier {
        return this.props.identifier
    }

    get description(): Description {
        return this.props.description
    }

    get serialNumber(): ElevatorSerialNumber {
        return this.props.serialNumber
    }

    set brand(newBrand: Brand) {
        this.props.brand = newBrand
    }

    set model(newModel: Model) {
        this.props.model = newModel
    }

    set serialNumber(newSerialNumber: ElevatorSerialNumber) {
        this.props.serialNumber = newSerialNumber
    }

    set description(newDescription: Description) {
        this.props.description = newDescription
    }

    set floors(newFloors: Floor[]) {
        this.props.floors = newFloors
    }
}
