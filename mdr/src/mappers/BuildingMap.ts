import { Mapper } from '../core/infra/Mapper'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'

import Building from '../domain/building/building'

import { BuildingCode as Code } from '../domain/building/code'
import { BuildingName as Name } from '../domain/building/name'
import { BuildingDescription as Description } from '../domain/building/description'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'

export class BuildingMap extends Mapper<Building> {
    static toDTO(building: Building): IBuildingDTO {
        const { length, width } = building.maxFloorDimensions
        return {
            code: building.code.value,
            name: building.name?.value,
            description: building.description?.value,
            maxFloorDimensions: { length, width },
        }
    }

    static toDomain(raw: any): Building {
        const code = Code.create(raw.code).getOrThrow()
        const name = raw.name && Name.create(raw.name).getOrThrow()
        const description =
            raw.description && Description.create(raw.description).getOrThrow()

        const maxFloorDimensions = MaxFloorDimensions.create(
            raw.maxFloorLength,
            raw.maxFloorWidth,
        ).getOrThrow()

        const result = Building.create(
            { code, name, description, maxFloorDimensions },
            new UniqueEntityID(raw.domainId),
        )

        if (result.isFailure) {
            return null
        }

        return result.getValue()
    }

    static toPersistence(building: Building): any {
        return {
            domainId: building.id.toString(),
            code: building.code.value,
            name: building.name?.value,
            description: building.description?.value,
            maxFloorLength: building.maxFloorDimensions.length,
            maxFloorWidth: building.maxFloorDimensions.width,
        }
    }
}
