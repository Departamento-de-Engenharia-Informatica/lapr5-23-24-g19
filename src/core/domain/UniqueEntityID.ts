
// remove by JRT : import uuid from 'uuid/v4';
const uuid = require('uuid').v4  // Added by JRT
import { Identifier } from './Identifier'

export class UniqueEntityID extends Identifier<string | number>{
  constructor (id?: string | number) {
    super(id ? id : uuid())
  }
}