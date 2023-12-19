import { assert } from 'chai'
import { describe, it } from 'mocha'
import { RoomDescription } from './description'

describe('Room Description', () => {
    it('should create a valid description', () => {
        const roomDescription = RoomDescription.create('RoomX')
        assert.isOk(roomDescription.isSuccess)
    })

    it('should fail when the description is null or undefined', () => {
        const roomDescription1 = RoomDescription.create(null)
        assert.isNotOk(roomDescription1.isSuccess)

        const roomDescription2 = RoomDescription.create(undefined)
        assert.isNotOk(roomDescription2.isSuccess)
    })

    it('should fail when the description exceeds the maximum length', () => {
        const longDescription = 'A'.repeat(251) // Exceeds the maximum length of 250 characters
        const roomDescription = RoomDescription.create(longDescription)

        assert.isNotOk(roomDescription.isSuccess)
    })
})
