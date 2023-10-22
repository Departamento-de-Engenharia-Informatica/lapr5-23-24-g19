import { Mapper } from '../core/infra/Mapper'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'

import Building from '../domain/building/building'

import { BuildingCode as Code } from '../domain/building/buildingCode'
import { BuildingName as Name } from '../domain/building/buildingName'
import { BuildingDescription as Description } from '../domain/building/description'
import { MaxFloorDimensions } from '../domain/building/maxFloorDimensions'

export class BuildingMap extends Mapper<Building> {
    public static toDTO(building: Building): IBuildingDTO {
        const { length, width } = building.maxFloorDimensions
        return {
            code: building.code.value,
            name: building.name.value,
            description: building.description.value,
            maxFloorDimensions: { length, width },
        }
    }

    public static async toDomain(raw: any): Promise<Building> {
        const code = Code.create(raw.code).getValue()
        const name = Name.create(raw.name).getValue()
        const description = Description.create(raw.description).getValue()

        const maxFloorDimensions = MaxFloorDimensions.create(raw.maxFloorLength, raw.maxFloorWidth).getValue()

        const result = Building.create(
            { code, name, description, maxFloorDimensions },
            new UniqueEntityID(raw.domainId),
        )

        if (result.isFailure) {
            console.log(result.error)
            return null
        }

        return result.getValue()
    }

    public static toPersistence(building: Building): any {
        return {
            domainId: building.id.toString(),
            code: building.code.value,
            name: building.name.value,
            description: building.description.value,
            maxFloorLength: building.maxFloorDimensions.length,
            maxFloorWidth: building.maxFloorDimensions.width,
        }
    }
}
