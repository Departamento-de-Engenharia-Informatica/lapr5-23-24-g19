import { AggregateRoot } from '../core/domain/AggregateRoot'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'

import { Result } from '../core/logic/Result'
import { BuildingId } from './buildingId'

import { IBuildingDTO } from '../dto/IBuildingDTO'
import { Guard } from '../core/logic/Guard'

interface BuildingProps {
    // código do edificio é obrigatório, no máximo 5 caracteres,letras e digitos, podendo conter espaços no meio
    code: string

    //nome do edificio é opcional, no máximo 50 caracteres alfanuméricos
    name: string

    //breve descrição,opcional, com o máximo de 255 caracteres
    description: string
}

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

    set name(value: string) {
        this.props.name = value
    }

    private constructor(props: BuildingProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(buildingDTO: IBuildingDTO, id?: UniqueEntityID): Result<Building> {
        const codeRegex = /^[A-Za-z][A-Za-z0-9 ]{,4}$/
        const code = buildingDTO.code
        const name = buildingDTO.name
        const description = buildingDTO.description

        if (!codeRegex.test(code)) {
            return Result.fail<Building>('Code of the building doesnt follow client specifications')
        }

        if (!!name === false || name.length > 50) {
            return Result.fail<Building>('Name of the building doesnt follow client specifications')
        }

        if (!!description === false || description.length > 255) {
            return Result.fail<Building>('Description of the building doesnt follow client specifications')
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
