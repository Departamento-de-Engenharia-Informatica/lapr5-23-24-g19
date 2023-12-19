import { assert } from 'chai'
import { describe, it } from 'mocha'
import { ElevatorBrand } from './brand'

describe('Elevator Brand', () => {
    it('cannot be longer than 50 characters', () => {
        const brands = [
            '8as9dg8aud90ag98fg9g89g9g9GF98G89G89G89GF8GF8Gf89GHFUIH98Gf89GFUDH9UH98F9',
            'A1b2C3d4E5f6G7h8I9j0K1l2M3n4O5p6Q7r8S9t0U1v2W3x4Y5z6A7B8C9D0E1F2G3H4I5J6K7L8M9N0O1P2Q3R4S5T6U7V8W9X0Y1Z2',
            'aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789abcdefghijklmnopqrstuvwx',
            '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
            'QwErTyUiOpAsDfGhJkLzXcVbNmQwErTyUiOpAsDfGhJkLzXcVbNm',
            'LdKjFa7p5MnHbP6gJqIv0wRtS1zTcN9XmY4z3V2K6aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789',
            '6oLg5iN8jMw9tOu0yPvX2s3HrIqZbCd7AeK6oLg5iN8jMw9tOu0yPvX2s3HrIqZbCd',
            'XxYyZzAaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWw1234567890XxYyZzAaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPp',
            'T7s8K9p0Rq1W2i3Y4u5Iv6Dw7Xe8F9z0G1h2J3k4L5m6N7o8991',
            'UvWw0XxYyZzAaBbCcDdEeFfGgHhIiJjKkLlMnOoPqRrSsTt1234567890UvWwXxYyZzAaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPqRrSsTt',
        ]

        brands.forEach((b) => assert.isNotOk(ElevatorBrand.create(b).isSuccess))

        // less than 50 chars
        assert.isOk(ElevatorBrand.create('KONE').isSuccess)
    })

    it('must be alphanumeric', () => {
        let brand = 'MitsubishiElectric'
        assert.isOk(ElevatorBrand.create(brand).isSuccess)

        brand = 'Schindler'
        assert.isOk(ElevatorBrand.create(brand).isSuccess)

        // dashes
        brand = 'Hyundai-Elevator'
        assert.isNotOk(ElevatorBrand.create(brand).isSuccess)

        brand = 'Delta95'
        assert.isOk(ElevatorBrand.create(brand).isSuccess)

        brand = 'Otis'
        assert.isOk(ElevatorBrand.create(brand).isSuccess)

        // clearly not valid
        brand = '!@#!$%!$!AQEQ@%[]1123'
        assert.isNotOk(ElevatorBrand.create(brand).isSuccess)
    })

    it('can contain spaces', () => {
        let brand = 'Mitsubishi Electric'
        assert.isOk(ElevatorBrand.create(brand).isSuccess)

        brand = 'Hyundai Elevator'
        assert.isOk(ElevatorBrand.create(brand).isSuccess)

        brand = 'TK Elevator'
        assert.isOk(ElevatorBrand.create(brand).isSuccess)
    })
})
