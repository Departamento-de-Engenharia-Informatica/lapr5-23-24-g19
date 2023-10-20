import { Mapper } from '../core/infra/Mapper'
import { IFloorDTO } from '../dto/IFloorDTO'
import { Floor } from '../domain/floor/floor'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { IFloorPersistence } from '../dataschema/IFloorPersistence'

export class FloorMap extends Mapper<Floor> {
    public static toDTO(Floor: Floor): IFloorDTO {
        return {
            buildingId: Floor.building.code,
            floorNumber: Floor.floorNumber.value,
            description: Floor.description.value,
        } as IFloorDTO
    }

    /*public static async toDomain(raw: any): Promise<Floor> {
        const FloorOrError = Floor.create(
            {
                buildingId: raw.building,
                floorNumber: raw.floorNumber,
                description: raw.description,
            },
            new UniqueEntityID(raw.domainId),
        )

        if (FloorOrError.isFailure) {
            console.log(FloorOrError.error)
            return null
        }

        return FloorOrError.isSuccess ? FloorOrError.getValue() : null
    }*/

    public static toPersistence(Floor: Floor): IFloorPersistence {
        return {
            _id: Floor.id.toString(),
            building: Floor.building.code,
            floorNumber: Floor.floorNumber.value,
            description: Floor.description.value,
        }
    }
}
