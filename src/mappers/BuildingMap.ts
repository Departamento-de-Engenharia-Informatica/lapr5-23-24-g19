import { Mapper } from '../core/infra/Mapper'
import { IBuildingDTO } from '../dto/IBuildingDTO'
import { Building } from '../domain/building/building'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'

export class BuildingMap extends Mapper<Building> {
    public static toDTO(building: Building): IBuildingDTO {
        return {
            //id: building.id.toString(),
            code: building.code,
            name: building.name,
            description: building.description,
            maxFloorDimensions: building.maxFloorDimensions,
        } as IBuildingDTO
    }

    public static async toDomain(raw: any): Promise<Building> {
        const buildingOrError = Building.create(
            {
                code: raw.code,
                name: raw.name,
                description: raw.description,
                maxFloorDimensions: {
                    length: raw.maxFloorLength,
                    width: raw.maxFloorWidth,
                },
            },
            new UniqueEntityID(raw.domainId),
        )

        if (buildingOrError.isFailure) {
            console.log(buildingOrError.error)
            return null
        }

        return buildingOrError.isSuccess ? buildingOrError.getValue() : null
    }

    public static toPersistence(building: Building): any {
        return {
            domainId: building.id.toString(),
            code: building.code,
            name: building.name,
            description: building.description,
            maxFloorLength: building.maxFloorDimensions.length,
            maxFloorWidth: building.maxFloorDimensions.width,
        }
    }
}
