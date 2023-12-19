import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import { Result } from '../../../core/logic/Result'
import { PhoneNumber } from '../phoneNumber'
import { VatNumber } from './vatNumber'
import { UserPassword } from '../userPassword'
import { Email } from '../email'
import Client from './Client'
import { Name } from '../name'

describe('Client create', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    afterEach(sinon.restore)

    it('should fail when creating a client with null or undefined email', () => {
        //stubCreate(Email)
        stubCreate(Name)
        stubCreate(PhoneNumber)
        stubCreate(VatNumber)
        stubCreate(UserPassword)

        const result = Client.create({
            email: undefined,
            name: Name.create('').getValue(),
            phoneNumber: PhoneNumber.create(0).getValue(),
            vatNumber: VatNumber.create(0).getValue(),
            password: UserPassword.create({ value: '', hashed: false }).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a client with null or undefined name', () => {
        stubCreate(Email)
        //stubCreate(Name)
        stubCreate(PhoneNumber)
        stubCreate(VatNumber)
        stubCreate(UserPassword)

        const result = Client.create({
            email: Email.create('').getValue(),
            name: undefined,
            phoneNumber: PhoneNumber.create(0).getValue(),
            vatNumber: VatNumber.create(0).getValue(),
            password: UserPassword.create({ value: '', hashed: false }).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a client with null or undefined phoneNumber', () => {
        stubCreate(Email)
        stubCreate(Name)
        //stubCreate(PhoneNumber)
        stubCreate(VatNumber)
        stubCreate(UserPassword)

        const result = Client.create({
            email: Email.create('').getValue(),
            name: Name.create('').getValue(),
            phoneNumber: undefined,
            vatNumber: VatNumber.create(0).getValue(),
            password: UserPassword.create({ value: '', hashed: false }).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a client with null or undefined vatNumber', () => {
        stubCreate(Email)
        stubCreate(Name)
        stubCreate(PhoneNumber)
        //stubCreate(VatNumber)
        stubCreate(UserPassword)

        const result = Client.create({
            email: Email.create('').getValue(),
            name: Name.create('').getValue(),
            phoneNumber: PhoneNumber.create(0).getValue(),
            vatNumber: undefined,
            password: UserPassword.create({ value: '', hashed: false }).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a client with null or undefined password', () => {
        stubCreate(Email)
        stubCreate(Name)
        stubCreate(PhoneNumber)
        stubCreate(VatNumber)
        //stubCreate(UserPassword)

        const result = Client.create({
            email: Email.create('').getValue(),
            name: Name.create('').getValue(),
            phoneNumber: PhoneNumber.create(0).getValue(),
            vatNumber: VatNumber.create(0).getValue(),
            password: undefined,
        })

        assert.isNotOk(result.isSuccess)
    })
})
