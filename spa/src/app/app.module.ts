import { NgModule } from '@angular/core';
import {HttpClientModule} from '@angular/common/http'
import { BrowserModule } from '@angular/platform-browser';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { BuildingComponent } from './components/building/building.component';
import { ElevatorComponent } from './components/elevator/elevator.component';
import { FloorComponent } from './components/floor/floor.component';
import { RoomComponent } from './components/room/room.component';
import { RobotComponent } from './components/robot/robot.component';
import { RobotTypeComponent } from './components/robot-type/robot-type.component';
import { PassageComponent } from './components/passage/passage.component';
import { PageNotFoundComponent } from './components/page-not-found/page-not-found.component';
import { Visualization3DComponent } from './components/visualization3-d/visualization3-d.component';
import { MainMenuComponent } from './components/main-menu/main-menu.component';
import { RouterModule } from '@angular/router';
import { AuthComponent } from './components/auth/auth.component';


@NgModule({
  declarations: [
    AppComponent,
    BuildingComponent,
    ElevatorComponent,
    FloorComponent,
    RoomComponent,
    RobotComponent,
    RobotTypeComponent,
    PassageComponent,
    PageNotFoundComponent,
    Visualization3DComponent,
    MainMenuComponent,
    AuthComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    HttpClientModule,
    RouterModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})


export class AppModule { }
