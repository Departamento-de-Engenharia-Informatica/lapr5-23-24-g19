import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import { BuildingId } from './buildingId'

import { BuildingName as Name } from './buildingName'
import { BuildingCode as Code } from './buildingCode'
import { Description } from '../description'
import { MaxFloorDimensions } from './maxFloorDimensions'

interface BuildingProps {
    code: Code
    name: Name
    description: Description
    maxFloorDimensions: MaxFloorDimensions
}

export default class Building extends AggregateRoot<BuildingProps> {
    get id(): UniqueEntityID {
        return this._id
    }

    get buildingId(): BuildingId {
        return BuildingId.caller(this.id)
    }

    get code(): Code {
        return this.props.code
    }

    get name(): Name {
        return this.props.name
    }

    get description(): Description {
        return this.props.description
    }

    get maxFloorDimensions(): MaxFloorDimensions {
        return this.props.maxFloorDimensions
    }

    private constructor(props: BuildingProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(dto: BuildingProps, id?: UniqueEntityID): Result<Building> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: dto.code, argumentName: 'buildingCode' },
            { argument: dto.name, argumentName: 'buildingName' },
            { argument: dto.description, argumentName: 'buildingDescription' },
            { argument: dto.maxFloorDimensions, argumentName: 'maxFloorDimensions' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else {
            return Result.ok(new Building({ ...dto }, id))
        }
    }
}
