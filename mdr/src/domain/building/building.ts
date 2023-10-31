import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import { BuildingId } from './buildingId'

import { BuildingName as Name } from './name'
import { BuildingCode as Code } from './code'
import { BuildingDescription as Description } from './description'
import { MaxFloorDimensions } from './maxFloorDimensions'

export interface BuildingProps {
    code: Code
    maxFloorDimensions: MaxFloorDimensions
    name?: Name
    description?: Description
}

export default class Building extends AggregateRoot<BuildingProps> {
    private constructor(props: BuildingProps, id?: UniqueEntityID) {
        super(props, id)
    }

    static create(dto: BuildingProps, id?: UniqueEntityID): Result<Building> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: dto.code, argumentName: 'buildingCode' },
            { argument: dto.maxFloorDimensions, argumentName: 'maxFloorDimensions' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else {
            return Result.ok(new Building({ ...dto }, id))
        }
    }

    fit(dimension: MaxFloorDimensions): boolean {
        return this.maxFloorDimensions.fit(dimension)
    }

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

    set name(newName: Name) {
        this.props.name = newName
    }

    get description(): Description {
        return this.props.description
    }

    set description(newDescription: Description) {
        this.props.description = newDescription
    }

    get maxFloorDimensions(): MaxFloorDimensions {
        return this.props.maxFloorDimensions
    }

    set maxFloorDimensions(newDimensions: MaxFloorDimensions) {
        this.props.maxFloorDimensions = newDimensions
    }
}
