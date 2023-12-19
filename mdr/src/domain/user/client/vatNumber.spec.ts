import { assert } from 'chai'
import { describe, it } from 'mocha'

import { VatNumber } from './vatNumber'

describe('VatNumber', () => {
    it('should have the correct length, equal to 9', () => {
        const vatNumber = VatNumber.create(123456789)
        assert.isOk(vatNumber.isSuccess)
    })

    it('length less than 9', () => {
        const vatNumber = VatNumber.create(12)
        assert.isNotOk(vatNumber.isSuccess)
    })

    it('length bigger than 9', () => {
        const vatNumber = VatNumber.create(12345678910)
        assert.isNotOk(vatNumber.isSuccess)
    })
})
