import { Mapper } from '../core/infra/Mapper'
import { IFloorDTO } from '../dto/IFloorDTO'
import { Passage } from '../domain/passage/passage'

import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { IPassagePersistence } from '../dataschema/mongo/IPassagePersistence'
import Container from 'typedi'
import { IPassageDTO } from '../dto/IPassageDTO'
import FloorRepo from '../repos/mongo/floorRepo'
import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'

export class PassageMap extends Mapper<Passage> {
    public static toDTO(passage: Passage): IPassageDTO {
        return {
            floor1: {
                buildingCode: passage.props.floor1.props.building.props.code.value,
                floorNumber: passage.props.floor1.props.floorNumber.value,
            },
            floor2: {
                buildingCode: passage.props.floor2.props.building.props.code.value,
                floorNumber: passage.props.floor2.props.floorNumber.value,
            },
        } as IPassageDTO
    }

    public static async toDomain(raw: IPassagePersistence): Promise<Passage> {
        const floorRepo = Container.get(FloorRepo)

        const floor1 = await floorRepo.findByID(raw.floor1ID)
        const floor2 = await floorRepo.findByID(raw.floor2ID)

        const passageOrError = Passage.create(
            { floor1, floor2 },
            new UniqueEntityID(raw.domainID),
        )
        return passageOrError.isSuccess ? passageOrError.getValue() : null
    }

    public static toPersistence(passage: Passage): IPassagePersistence {
        return {
            domainID: passage.id.toString(),
            floor1ID: passage.props.floor1.id.toString(),
            floor2ID: passage.props.floor2.id.toString(),
        }
    }
}
