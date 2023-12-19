import { Inject, Service } from 'typedi'
import { Model, Document } from 'mongoose'
import config from '../../../config'
import IElevatorPersistence from '../../dataschema/mongo/IElevatorPersistence'
import Building from '../../domain/building/building'
import Elevator from '../../domain/elevator/Elevator'
import IElevatorRepo, { ElevatorDataMap } from '../../services/IRepos/IElevatorRepo'
import MongoElevatorDataMap from './dataMapper/ElevatorDataMap'
import { ElevatorIdentifier } from '../../domain/elevator/identifier'

@Service()
export default class ElevatorRepo implements IElevatorRepo {
    private readonly mapper: ElevatorDataMap<IElevatorPersistence>

    constructor(
        @Inject(config.schemas.elevator.name)
        private schema: Model<IElevatorPersistence & Document>,
    ) {
        // this shouldn't be needed; check class impl for more info
        this.mapper = new MongoElevatorDataMap()
    }

    async save(t: Elevator): Promise<Elevator> {
        const query: Partial<IElevatorPersistence> = {
            building: t.building.code.value,
            identifier: t.identifier.value,
        }

        const doc = await this.schema.findOne(query)
        try {
            const raw = await this.mapper.toPersistence(t)
            if (!doc) {
                const elevator = await this.schema.create(raw)

                return this.mapper.toDomain(elevator)
            } else {
                doc.floors = raw.floors
                doc.brand = raw.brand
                doc.model = raw.model
                doc.serialNumber = raw.serialNumber
                doc.description = raw.description

                await doc.save()
                return t
            }
        } catch (err) {
            throw err
        }
    }
    async exists(t: Elevator): Promise<boolean> {
        return this.existsInBuilding(t.building, t.identifier)
    }

    async inBuilding(building: Building): Promise<Elevator[]> {
        const result = await this.schema.find({
            building: building.code.value,
        })

        return Promise.all(result.map(async (e) => await this.mapper.toDomain(e))) ?? []
    }

    async existsInBuilding(
        building: Building,
        identifier: ElevatorIdentifier,
    ): Promise<boolean> {
        const query: Partial<IElevatorPersistence> = {
            building: building.code.value,
            identifier: identifier.value,
        }

        const doc = await this.schema.findOne(query)

        return !!doc === true
    }

    async findByIdentifier(
        building: Building,
        identifier: ElevatorIdentifier,
    ): Promise<Elevator> {
        const query: Partial<IElevatorPersistence> = {
            building: building.code.value,
            identifier: identifier.value,
        }

        const doc = await this.schema.findOne(query)

        return this.mapper.toDomain(doc)
    }

    async nextIdentifier(): Promise<ElevatorIdentifier> {
        const count = (await this.schema.find()).length

        const identifier = ElevatorIdentifier.create(count + 1)
        if (identifier.isFailure) return Promise.reject('Error generating identifier')
        return identifier.getValue()
    }
}
