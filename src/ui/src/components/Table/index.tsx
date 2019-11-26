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
    width: '40%',
    height: '60%',
    overflowX: 'auto',
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
  const keys = Array.from(new Set(Object.keys(rows)))
  console.log(keys)
  console.log(rows)
  return (
    <Paper className={classes.root}>
      <Table className={classes.table} aria-label="simple table">
        <TableHead>
          <TableRow>
            <TableCell>Day</TableCell>
            <TableCell>Balance</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
        {rows.map((row, i) => (
            <TableRow key={`${i}`}>
              <TableCell component="th" scope="row">
                {Object.keys(row)[0]}
              </TableCell>
              <TableCell component="th" scope="row">
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