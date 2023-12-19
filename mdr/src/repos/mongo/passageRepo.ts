import { Service, Inject, Container } from 'typedi'
import { Document, Model } from 'mongoose'
import { IPassagePersistence } from '../../dataschema/mongo/IPassagePersistence'
import IPassageRepo from '../../services/IRepos/IPassageRepo'
import { Passage } from '../../domain/passage/passage'
import { PassageMap } from '../../mappers/PassageMap'
import { Floor } from '../../domain/floor/floor'
import config from '../../../config'
import IFloorRepo from '../../services/IRepos/IFloorRepo'
import { IFloorPassageDomainDTO } from '../../dto/IFloorPassageDomainDTO'
import Building from '../../domain/building/building'

@Service()
export default class PassageRepo implements IPassageRepo {
    constructor(
        @Inject('passageSchema')
        private passageSchema: Model<IPassagePersistence & Document>,
    ) {}

    async exists(passage: Passage | string): Promise<boolean> {
        if (passage instanceof Passage) {
            const query = {
                floor1ID: passage.props.floor1.id.toString(),
                floor2ID: passage.props.floor2.id.toString(),
            }
            const passageDocument = await this.passageSchema.findOne(query)

            try {
                if (!!passageDocument) {
                    return true
                }

                //ordem 1 nao existe
                const query2 = {
                    floor1ID: passage.props.floor2.id.toString(),
                    floor2ID: passage.props.floor1.id.toString(),
                }
                const passageDocument2 = await this.passageSchema.findOne(query2)

                return !!passageDocument2
            } catch (error) {
                throw error
            }
        }
        return false
    }

    async save(passage: Passage): Promise<Passage> {
        const query = { domainID: passage.id.toString() }
        const passageDocument = await this.passageSchema.findOne(query)
        const rawPassage = PassageMap.toPersistence(passage)

        try {
            if (passageDocument === null) {
                const passageCreated = await this.passageSchema.create(rawPassage)
                return PassageMap.toDomain(passageCreated)
            } else {
                passageDocument.floor1ID = rawPassage.floor1ID
                passageDocument.floor2ID = rawPassage.floor2ID
                await passageDocument.save()

                return PassageMap.toDomain(passageDocument)
            }
        } catch (err) {
            throw err
        }
    }

    async findAll(): Promise<Passage[]> {
        const records = await this.passageSchema.find()

        if (records.length === 0) {
            return []
        }

        return Promise.all(
            records.map(async (record) => await PassageMap.toDomain(record)),
        )
    }

    async passagesBetweenBuildings(b1: Building, b2: Building): Promise<Passage[]> {
        try {
            const allPassages = await this.findAll()

            const buildingCode1 = b1.code.value
            const buildingCode2 = b2.code.value

            const passages = allPassages
                .filter(
                    (info) =>
                        (info.props.floor1.building.code.value === buildingCode1 &&
                            info.props.floor2.building.code.value === buildingCode2) ||
                        (info.props.floor1.building.code.value === buildingCode2 &&
                            info.props.floor2.building.code.value === buildingCode1),
                )
                .map((info) => info.id.toString())

            const docs = await this.passageSchema.find({
                domainID: { $in: passages },
            })

            const passageList = await Promise.all(
                docs.map(async (d) => await PassageMap.toDomain(d)),
            )

            return passageList
        } catch (error) {
            throw error
        }
    }

    async floorsWithPassage(building: Building): Promise<IFloorPassageDomainDTO[]> {
        type QueryResult = { matchedField: { from: string; destinations: string[] } }

        const floorRepo = Container.get(config.repos.floor.name) as IFloorRepo
        const buildingFloors = await floorRepo.findAllInBuilding(building)
        const floorIDs = buildingFloors.map((f) => f.id.toString())

        const results: QueryResult[] = await this.passageSchema.aggregate([
            {
                $match: {
                    $or: [
                        { floor1ID: { $in: floorIDs } },
                        { floor2ID: { $in: floorIDs } },
                    ],
                },
            },
            {
                $project: {
                    _id: 0,
                    matchedField: {
                        $cond: {
                            if: { $in: ['$floor1ID', floorIDs] },
                            then: { from: '$floor1ID', to: '$floor2ID' },
                            else: { from: '$floor2ID', to: '$floor1ID' },
                        },
                    },
                },
            },
            {
                $group: {
                    _id: '$matchedField.from',
                    destinations: { $addToSet: '$matchedField.to' },
                },
            },
            {
                $project: {
                    _id: 0,
                    matchedField: {
                        from: '$_id',
                        destinations: '$destinations',
                    },
                },
            },
        ])

        return Promise.all(
            results.map(async ({ matchedField: connections }) => {
                return {
                    from: buildingFloors.find(
                        (f) => f.id.toString() === connections.from,
                    ),
                    destinations: await Promise.all(
                        connections.destinations.map(
                            async (f) => await floorRepo.findById(f),
                        ),
                    ),
                }
            }),
        )
    }

    async find(floor1: Floor, floor2: Floor): Promise<Passage> {
        return PassageMap.toDomain(
            (await this.passageSchema.findOne({
                floor1ID: floor1.id.toString(),
                floor2ID: floor2.id.toString(),
            })) ??
                (await this.passageSchema.findOne({
                    floor1ID: floor2.id.toString(),
                    floor2ID: floor1.id.toString(),
                })),
        )
    }
}
