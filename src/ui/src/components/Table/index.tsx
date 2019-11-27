import React, {FC} from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';
import Paper from '@material-ui/core/Paper';

const useStyles = makeStyles({
  root: {
    width: '50%',
    height: '50%',
    overflowX: 'auto',
    marginTop: 30
  },
  table: {
    minWidth: 350,
  },
});

type Props = {
  rows: any
}

const SimpleTable:FC<Props> = ({rows}) => {
  const classes = useStyles({});

  return (
    <Paper className={classes.root}>
      <Table className={classes.table} aria-label="simple table">
        <TableHead>
          <TableRow>
            <TableCell align="center">Day</TableCell>
            <TableCell align="center">Balance</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
        {rows.map((row, i) => (
            <TableRow key={`${i}`}>
              <TableCell align="center" component="th" scope="row">
                {Object.keys(row)[0]}
              </TableCell>
              <TableCell align="center"  component="th" scope="row">
                {Object.values(row)[0]}
              </TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </Paper>
  );
}

export default SimpleTable