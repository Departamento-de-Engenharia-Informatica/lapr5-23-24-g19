import Container from 'typedi'
import config from '../../../../config'
import { UniqueEntityID } from '../../../core/domain/UniqueEntityID'
import IElevatorPersistence from '../../../dataschema/mongo/IElevatorPersistence'
import { BuildingCode } from '../../../domain/building/code'
import { ElevatorBrand as Brand } from '../../../domain/elevator/brand'
import { ElevatorDescription as Description } from '../../../domain/elevator/description'
import Elevator from '../../../domain/elevator/Elevator'
import { ElevatorIdentifier as Identifier } from '../../../domain/elevator/identifier'
import { ElevatorModel as Model } from '../../../domain/elevator/model'
import { ElevatorSerialNumber as SerialNumber } from '../../../domain/elevator/serialNumber'
import { FloorNumber } from '../../../domain/floor/floorNumber'
import IBuildingRepo from '../../../services/IRepos/IBuildingRepo'
import { ElevatorDataMap } from '../../../services/IRepos/IElevatorRepo'
import IFloorRepo from '../../../services/IRepos/IFloorRepo'

// scuffed impl because of schema not using ObjectId's
export default class MongoElevatorDataMap
    implements ElevatorDataMap<IElevatorPersistence>
{
    private buildingRepo: IBuildingRepo
    private floorRepo: IFloorRepo

    constructor() {
        this.buildingRepo = Container.get(config.repos.building.name)
        this.floorRepo = Container.get(config.repos.floor.name)
    }

    async toPersistence(d: Elevator): Promise<IElevatorPersistence> {
        return {
            domainId: d.id.toString(),
            building: d.building.code.value,
            identifier: d.identifier.value,
            floors: d.floors.map((f) => f.floorNumber.value),
            brand: d.brand?.value,
            model: d.model?.value,
            serialNumber: d.serialNumber?.value,
            description: d.description?.value,
        }
    }
    async toDomain(p: IElevatorPersistence): Promise<Elevator> {
        const building = await this.buildingRepo.findByCode(
            BuildingCode.create(p.building).getValue(),
        )
        const floors = await Promise.all(
            p.floors.map(async (f) => {
                return await this.floorRepo.find(
                    building,
                    FloorNumber.create(f).getValue(),
                )
            }),
        )

        const identifier = Identifier.create(p.identifier).getValue()

        const brand = p.brand && Brand.create(p.brand).getValue()
        const model = p.model && Model.create(p.model).getValue()
        const serialNumber =
            p.serialNumber && SerialNumber.create(p.serialNumber).getValue()
        const description = p.description && Description.create(p.description).getValue()

        const result = Elevator.create(
            {
                building,
                identifier,
                floors,

                brand,
                model,
                serialNumber,
                description,
            },
            new UniqueEntityID(p.domainId),
        )

        return result.isSuccess ? result.getValue() : null
    }
}
