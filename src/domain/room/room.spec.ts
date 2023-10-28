import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'
import Room from './room'
import { RoomName } from './roomName'
import { Result } from '../../core/logic/Result'
import { RoomDescription } from './description'
import { RoomDimensions } from './roomDimensions'
import { RoomCategory } from './roomCategory'
import { Coordinates } from '../floor/Coordinates'
import { Floor, FloorProps } from '../floor/floor'

describe('Room', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    /*let name: RoomName,
        description: RoomDescription,
        dimensions: RoomDimensions,
        category: RoomCategory,
        positions: Coordinates*/

    let floor: Floor

    beforeEach(() => {
        stubCreate(Floor)

        floor = Floor.create({} as FloorProps).getValue()
    })

    afterEach(sinon.restore)

    it('if now of the parameters are null or undefined', () => {
        const name = RoomName.create('Room X').getValue()
        const description = RoomDescription.create('A test room').getValue()
        const dimensions = RoomDimensions.create(5, 3).getValue()
        const category = RoomCategory.create('GABINETE').getValue()
        const positions = Coordinates.create(1, 2).getValue()

        const result = Room.create({
            name,
            category,
            description,
            floor,
            dimensions,
            positions,
        })

        assert.isOk(result.isSuccess)
    })

    it('room name null or undefined', () => {
        stubCreate(RoomCategory)
        stubCreate(RoomDescription)
        stubCreate(RoomDimensions)
        stubCreate(Coordinates)

        const result = Room.create({
            name: undefined,
            category: RoomCategory.create('').getValue(),
            description: RoomDescription.create('').getValue(),
            floor,
            dimensions: RoomDimensions.create(0, 0).getValue(),
            positions: Coordinates.create(0, 0).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('room category null or undefined', () => {
        stubCreate(RoomName)
        stubCreate(RoomDescription)
        stubCreate(RoomDimensions)
        stubCreate(Coordinates)

        const result = Room.create({
            name: RoomName.create('').getValue(),
            category: undefined,
            description: RoomDescription.create('').getValue(),
            floor,
            dimensions: RoomDimensions.create(0, 0).getValue(),
            positions: Coordinates.create(0, 0).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('room description null or undefined', () => {
        stubCreate(RoomName)
        stubCreate(RoomCategory)
        stubCreate(RoomDimensions)
        stubCreate(Coordinates)

        const result = Room.create({
            name: RoomName.create('').getValue(),
            category: RoomCategory.create('').getValue(),
            description: undefined,
            floor,
            dimensions: RoomDimensions.create(0, 0).getValue(),
            positions: Coordinates.create(0, 0).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('room dimensions null or undefined', () => {
        stubCreate(RoomName)
        stubCreate(RoomCategory)
        stubCreate(RoomDescription)
        stubCreate(Coordinates)

        const result = Room.create({
            name: RoomName.create('').getValue(),
            category: RoomCategory.create('').getValue(),
            description: RoomDescription.create('').getValue(),
            floor,
            dimensions: undefined,
            positions: Coordinates.create(0, 0).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('room positions null or undefined', () => {
        stubCreate(RoomName)
        stubCreate(RoomCategory)
        stubCreate(RoomDescription)
        stubCreate(RoomDimensions)

        const result = Room.create({
            name: RoomName.create('').getValue(),
            category: RoomCategory.create('').getValue(),
            description: RoomDescription.create('').getValue(),
            floor,
            dimensions: RoomDimensions.create(0, 0).getValue(),
            positions: undefined,
        })

        assert.isNotOk(result.isSuccess)
    })
})
