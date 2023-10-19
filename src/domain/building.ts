import { AggregateRoot } from '../core/domain/AggregateRoot'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'

import { Result } from '../core/logic/Result'
import { BuildingId } from './buildingId'

import { IBuildingDTO } from '../dto/IBuildingDTO'
import { Guard } from '../core/logic/Guard'

type Dimensions = { length: number, width: number }

interface BuildingProps {
    // max 5 alphanumeric+spaces chars
    code: string

    // max 50 alphanumeric characters
    name?: string

    // max 255 chars
    description?: string

    maxFloorDimensions: Dimensions
}

const codeRegex = /^[A-Za-z ]{1,5}$/
const maxNameLength = 50
const maxDescriptionLength = 255

export class Building extends AggregateRoot<BuildingProps> {
    get id(): UniqueEntityID {
        return this._id
    }

    get buildingId(): BuildingId {
        return BuildingId.caller(this.id)
    }

    get code(): string {
        return this.props.code
    }

    get name(): string {
        return this.props.name
    }

    get description(): string {
        return this.props.description
    }

    get maxFloorDimensions() : Dimensions {
        return { ...this.props.maxFloorDimensions }
    }

    private constructor(props: BuildingProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(buildingDTO: IBuildingDTO, id?: UniqueEntityID): Result<Building> {
        const code = buildingDTO.code.trim()
        const name = buildingDTO.name
        const description = buildingDTO.description

        if (!codeRegex.test(code)) {
            return Result.fail<Building>(
                'Code must contain at most 5 characters, letters and numbers, ' +
                'possibly with spaces in-between'
            )
        }


        if (!!name === false || name.length > maxNameLength) {
            return Result.fail<Building>(
                `The name of the building must have at most ${maxNameLength} characters`
            )
        }

        if (!!description === false || description.length > maxDescriptionLength) {
            return Result.fail<Building>(
                `The Description must be at most ${maxDescriptionLength} characters`
            )
        }

        const guardedProps = [
            { argument: buildingDTO.code, argumentName: 'buildingCode' },
            { argument: buildingDTO.name, argumentName: 'buildingName' },
            { argument: buildingDTO.description, argumentName: 'buildingDescription' },
        ]

        const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps)
        if (!guardResult.succeeded) {
            return Result.fail<Building>(guardResult.message)
        } else {
            const building = new Building({ ...buildingDTO }, id)

            return Result.ok<Building>(building)
        }
    }
}
