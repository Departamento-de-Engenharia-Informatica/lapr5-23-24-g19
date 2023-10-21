import { Mapper } from '../core/infra/Mapper'
import { IFloorDTO } from '../dto/IFloorDTO'
import { Floor } from '../domain/floor/floor'
import { FloorNumber } from '../domain/floor/floorNumber'
import { BuildingCode } from '../domain/building/buildingCode'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { IFloorPersistence } from '../dataschema/IFloorPersistence'
import Container from 'typedi'
import BuildingRepo from '../repos/buildingRepo'
import { Description } from '../domain/description'

export class FloorMap extends Mapper<Floor> {
    public static toDTO(Floor: Floor): IFloorDTO {
        return {
            buildingCode: Floor.building.code.value,
            floorNumber: Floor.floorNumber.value,
            description: Floor.description.value,
        } as IFloorDTO
    }

    public static async toDomain(raw: any): Promise<Floor> {
        const buildingRepo = Container.get(BuildingRepo)
        const building = await buildingRepo.findByCode(BuildingCode.create(raw.buildingCode).getValue())
        const FloorOrError = Floor.create(
            {
                floorNumber: FloorNumber.create(raw.floorNumber).getValue(),
                description: Description.create(raw.description).getValue(),
                building: building,
            },
            new UniqueEntityID(raw.domainId)
        )

        if (FloorOrError.isFailure) {
            console.log(FloorOrError.error)
            return null
        }

        return FloorOrError.isSuccess ? FloorOrError.getValue() : null
    }

    public static toPersistence(Floor: Floor): IFloorPersistence {
        return {
            domainId: Floor.id.toString(),
            buildingCode: Floor.building.code.value,
            floorNumber: Floor.floorNumber.value,
            description: Floor.description.value,
        }
    }
}
